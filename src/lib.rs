//! # region_buffer
//! A growable array allowing for multiple mutable non overlapping regions from
//! the same `Vec`.
//!
//! # Examples
//! ```
//!# use region_buffer::RegionBuffer;
//! let mut buffer = RegionBuffer::new();
//!
//! buffer.push(1);
//! buffer.push(2);
//!
//! assert_eq!(buffer.len(), 2);
//!
//! let mut a = buffer.get_mut(0);
//! let mut b = buffer.get_mut(1);
//!
//! assert_eq!(*a, 1);
//! assert_eq!(*b, 2);
//!
//! *a = *b;
//! *b = 3;
//!
//! assert_eq!(*a, 2);
//! assert_eq!(*b, 3);
//! ```
//! There is a [`region_buffer`] macro provided to make initialisation more
//! convenient.
//! ```
//!# #[macro_use]
//!# extern crate region_buffer;
//!
//!# fn main() {
//! let strings = region_buffer!["Hello", "World", "!"];
//!
//! let mut greeting =  strings.get_mut(0);
//! let mut noun =  strings.get_mut(1);
//! let mut punctuation =  strings.get_mut(2);
//!
//! *greeting = "Hallo";
//! *noun = "Peter";
//! *punctuation = ".";
//!
//! let string = format!("{} {}{}", greeting, noun, punctuation);
//! assert_eq!(string, "Hallo Peter.")
//!# }
//! ```
//! The macro can also be used to specify and initialise large regions of
//! memory.
//! ```
//!# #[macro_use]
//!# extern crate region_buffer;
//!#
//!# type Slice<'a> = region_buffer::Slice<'a, u8>;
//!#
//!# struct Console;
//!# impl Console {
//!#   fn new(_: Slice, _: Slice, _: Slice, _: Slice) {}
//!# }
//!#
//!# fn main() {
//! let memory = region_buffer![0; 0xFFFF];
//!
//! let rom =  memory.region(0, 0x800);
//! let gpu = memory.region(0x800, 0x1600);
//! let sound = memory.region(0x1600, 0x2400);
//! let ram = memory.region(0x2400, 0xFFFF);
//!
//! let console = Console::new(rom, gpu, sound, ram);
//!# }
//! ```
//!
//! [`region_buffer`]: macro.region_buffer.html

#![deny(missing_docs)]

use std::cell::UnsafeCell;
use std::cmp::PartialEq;
use std::collections::HashSet;
use std::fmt;
use std::ops::{Deref, DerefMut, Index, IndexMut};
use std::sync::RwLock;

/// Creates a [`RegionBuffer`] containing the arguments.
/// `region_buffer!` allows [`RegionBuffer`]s to be defined with the same syntax
/// as array expressions. There are two forms of this macro:
///
/// * Create a [`RegionBuffer`] containing a given list of elements:
/// ```
///# #[macro_use] extern crate region_buffer;
/// let buf = region_buffer![1, 2, 3];
///
/// assert_eq!(*buf.get(0), 1);
/// assert_eq!(*buf.get(1), 2);
/// assert_eq!(*buf.get(2), 3);
/// ```
///
/// * Create a [`RegionBuffer`] given element and length:
/// ```
///# #[macro_use] extern crate region_buffer;
/// let buf = region_buffer![1; 3];
///
/// assert_eq!(*buf.region(0, 3), [1, 1, 1]);
/// ```
///
/// Note that unlike array expressions this syntax supports all elements which
/// implement `Clone` and the number of elements doesn't have to be a constant.
/// This will use clone to duplicate an expression, so one should be careful
/// using this with types having a nonstandard `Clone` implementation.
///
/// For example, `region_buffer![Rc::new(1); 5]` will create a vector of five
/// references to the same boxed integer value, not five references pointing to
/// independently boxed integers.
///
/// [`RegionBuffer`]: struct.RegionBuffer.html
#[macro_export]
macro_rules! region_buffer {
    ($($element:expr),*) => {{
        let mut region_buffer = $crate::RegionBuffer::new();

        $(
            region_buffer.push($element);
        )*

        region_buffer
    }};

    ($element:expr; $len:expr) => {{
        $crate::RegionBuffer::from_elements($element, $len)
    }}
}

/// A contiguous growable array type, allow you to obtain multiple mutable
/// regions from it, as long these regions don't overlap.
#[derive(Debug, Default)]
pub struct RegionBuffer<T> {
    region: UnsafeCell<Vec<T>>,
    ranges: Ranges,
}

#[allow(clippy::new_without_default_derive)]
impl<T> RegionBuffer<T> {

    /// Constructs a new, empty `RegionBuffer<T>`.  The buffer will not allocate
    /// until elements are pushed onto it.
    pub fn new() -> Self
    {
        Self {
            region: UnsafeCell::new(Vec::new()),
            ranges: Ranges::new(),
        }
    }

    /// Appends an element to the back of a collection.
    ///
    /// # Panics
    /// Panics if the number of elements in the buffer overflows a `usize`.
    pub fn push(&mut self, element: T) {
        unsafe { &mut *self.region.get() }.push(element)
    }

    /// Returns the number of elements in the buffer, also referred to as its
    /// 'length'.
    pub fn len(&self) -> usize {
        unsafe { &*self.region.get() }.len()
    }

    /// Returns `true` if the region buffer contains no elements.
    pub fn is_empty(&self) -> bool {
        unsafe { &*self.region.get() }.is_empty()
    }

    /// Shortens the buffer, keeping the first `len` elements and dropping the
    /// rest.  If `len` is greater than the buffer's current length, this has no
    /// effect.
    ///
    /// Note that this method has no effect on the allocated capacity of the
    /// buffer.
    ///
    /// # Panics
    /// If `len` is less than an already borrowed region.
    pub fn truncate(&self, len: usize) {
        assert!(
            self.ranges.0.read().unwrap().0.iter().all(|(_, end)| len > *end),
            "Truncated into an already borrowed region"
        );
        unsafe { &mut *self.region.get() }.truncate(len)
    }

    /// Provides a mutable reference to a region in the buffer, provided that
    /// region hasn't already been borrowed.
    ///
    /// # Panics
    /// If the region has already been borrowed.
    pub fn region(&self, start: usize, end: usize) -> Slice<T> {
        self.ranges.insert(start, end);

        let data = &mut unsafe { &mut *self.region.get() }[start..end];

        Slice::new(data, (start, end), &self.ranges)
    }

    /// Returns a single element from the buffer. The borrowing rules also apply
    /// to this single element, so you can't get a single element from an
    /// already borrowed region and vice versa.
    pub fn get(&self, index: usize) -> Element<T> {
        self.ranges.insert(index, index + 1);

        Element::new(&unsafe { &*self.region.get() }[index], index, &self.ranges)
    }

    /// Returns a single mutable element from the buffer. The borrowing rules
    /// also apply to this single element, so you can't get a single element
    /// from an already borrowed region and vice versa.
    pub fn get_mut(&self, index: usize) -> ElementMut<T> {
        self.ranges.insert(index, index + 1);

        ElementMut::new(&mut unsafe { &mut *self.region.get() }[index], index, &self.ranges)
    }

}

unsafe impl<T> Sync for RegionBuffer<T> {}

impl<T: Clone> RegionBuffer<T> {
    /// Initialise a buffer of `len` size, with all elements initialised to
    /// `element`.
    pub fn from_elements(element: T, len: usize) -> Self {
        Self {
            region: UnsafeCell::new(vec![element; len]),
            ranges: Ranges::new(),
        }
    }

    /// Expands the region by `to` size, with all new elements initialised to
    /// `element`.
    pub fn expand(&mut self, to: usize, element: T) {
        let region = unsafe { &mut *self.region.get() };
        region.reserve(to + 1);
        for _ in 0..to {
            region.push(element.clone());
        }
    }

}

/// Represents a mutable slice into a region of memory. The region will be freed
/// on `Drop`.
#[derive(Debug)]
pub struct Slice<'a, T: 'a> {
    data: &'a mut [T],
    points: (usize, usize),
    ranges: &'a Ranges,
}

impl<'a, T: 'a> Slice<'a, T> {
    fn new(data: &'a mut [T], points: (usize, usize), ranges: &'a Ranges) -> Self {
        Self { data, points, ranges }
    }
}

impl<'a, T: 'a> Drop for Slice<'a, T> {
    fn drop(&mut self) {
        self.ranges.0.write().unwrap().0.remove(&self.points);
    }
}

impl<'a, T: 'a> Deref for Slice<'a, T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &*self.data
    }
}

impl<'a, T: 'a> DerefMut for Slice<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.data
    }
}

impl<'a, T> Index<usize> for Slice<'a, T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        &self.data[index]
    }
}

impl<'a, T> IndexMut<usize> for Slice<'a, T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.data[index]
    }
}

impl<'a, T: PartialEq> PartialEq<Slice<'a, T>> for Slice<'a, T> {
    fn eq(&self, rhs: &Self) -> bool {
        self.data == rhs.data
    }
}

impl<'a, T: PartialEq> PartialEq<[T]> for Slice<'a, T> {
    fn eq(&self, rhs: &[T]) -> bool {
        self.data == rhs
    }
}

impl<'a, 'b, T: PartialEq> PartialEq<&'b mut [T]> for Slice<'a, T> {
    fn eq(&self, rhs: &&mut [T]) -> bool {
        self.data == *rhs
    }
}

/// A struct representing a single element from the `RegionBuffer`. When
/// `Drop`ped the element's solitary region is freed.
pub struct Element<'a, T: 'a> {
    data: &'a T,
    index: usize,
    parent: &'a Ranges,
}

impl<'a, T> Element<'a, T> {
    fn new(data: &'a T, index: usize, parent: &'a Ranges) -> Self {
        Self { data, index, parent }
    }
}

impl<'a, T> Deref for Element<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.data
    }
}

impl<'a, T> Drop for Element<'a, T> {
    fn drop(&mut self) {
        self.parent.0.write().unwrap().0.remove(&(self.index, self.index + 1));
    }
}

impl<'a, T: fmt::Debug> fmt::Debug for Element<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.data.fmt(f)
    }
}

impl<'a, T: fmt::Display> fmt::Display for Element<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.data.fmt(f)
    }
}

/// A struct representing a single mutable element from the `RegionBuffer`. When
/// `Drop`ped the element's solitary region is freed.
pub struct ElementMut<'a, T: 'a> {
    data: &'a mut T,
    index: usize,
    parent: &'a Ranges,
}

impl<'a, T> ElementMut<'a, T> {
    fn new(data: &'a mut T, index: usize, parent: &'a Ranges) -> Self {
        Self { data, index, parent }
    }
}

impl<'a, T> Deref for ElementMut<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.data
    }
}

impl <'a, T> DerefMut for ElementMut<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.data
    }
}

impl<'a, T> Drop for ElementMut<'a, T> {
    fn drop(&mut self) {
        self.parent.0.write().unwrap().0.remove(&(self.index, self.index + 1));
    }
}

impl<'a, T: fmt::Debug> fmt::Debug for ElementMut<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.data.fmt(f)
    }
}

impl<'a, T: fmt::Display> fmt::Display for ElementMut<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.data.fmt(f)
    }
}

#[derive(Debug, Default)]
struct Ranges(RwLock<RangesInner>);

impl Ranges {
    fn new() -> Self {
        Ranges(RwLock::new(RangesInner::new()))
    }

    fn insert(&self, start: usize, end: usize) {
        let mut locked = self.0.write().unwrap();
        if let Err(e) = locked.assert_region_is_free(start, end) {
            // Dropping to prevent poisoning
            drop(locked);
            panic!(e);
        }
        locked.0.insert((start, end));
    }
}

#[derive(Debug, Default)]
struct RangesInner(HashSet<(usize, usize)>);

impl RangesInner {
    fn new() -> Self {
        RangesInner(HashSet::new())
    }

    fn assert_region_is_free(&self, start: usize, end: usize) -> Result<(), String> {
        match self.is_region_borrowed(start, end) {
            Overlaps::None => Ok(()),
            error => Err(error.to_string())
        }
    }

    fn is_region_borrowed(&self, start: usize, end: usize) -> Overlaps {
        for (used_start, used_end) in self.0.iter().map(|(x, y)| (*x, *y)) {
            if start >= used_start && start < used_end
            {
                return Overlaps::Start
            } else if end >= used_start && end <= used_end
            {
                return Overlaps::End
            } else if used_start >= start && used_end <= end {
                return Overlaps::StartAndEnd
            }
        }

        Overlaps::None
    }
}

#[derive(Debug, PartialEq)]
enum Overlaps {
    None,
    Start,
    End,
    StartAndEnd,
}

impl fmt::Display for Overlaps {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Overlaps::None => "No Overlapping regions",
            Overlaps::Start => "Start overlaps into borrowed region",
            Overlaps::End => "End overlaps into borrowed region",
            Overlaps::StartAndEnd => "Start and end overlaps into borrowed \
                region",
        };

        s.fmt(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_use() {
        let foo = region_buffer![0; 0x800];

        println!("{:?}", foo);

        let mut region_a = foo.region(0, 0x400);

        for i in 0..0x400 {
            region_a[i] = i;
        }

        let mut region_b = foo.region(0x400, 0x800);

        for i in (0..0x400).rev() {
            region_b[i] = i;
        }

        region_b.sort();

        assert_eq!(region_a, region_b);
    }

    #[test]
    #[should_panic(expected="Start overlaps into borrowed region")]
    fn starts_in_overlap() {
        let foo = region_buffer![0; 0x800];

        let _a = foo.region(0, 0x400);
        let _b = foo.region(0, 0x300);
    }

    #[test]
    #[should_panic(expected="End overlaps into borrowed region")]
    fn ends_in_overlap() {
        let foo = region_buffer![0; 0x800];

        let _a = foo.region(0x300, 0x400);
        let _b = foo.region(0, 0x350);
    }

    #[test]
    #[should_panic(expected="Start and end overlaps into borrowed region")]
    fn completely_overlaps() {
        let foo = region_buffer![0; 0x800];

        let _a = foo.region(0x300, 0x400);
        let _b = foo.region(0, 0x800);
    }

    #[test]
    fn dropped_region() {
        let foo = region_buffer![0; 0x800];

        let _ = foo.region(0, 0x400);
        let _ = foo.region(0, 0x400);
    }

    #[test]
    fn truncate() {
        let foo = region_buffer![1, 2, 3];

        let _a = foo.get_mut(0);

        foo.truncate(2);
    }

    #[test]
    #[should_panic(expected="Truncated into an already borrowed region")]
    fn truncate_into_borrowed() {
        let foo = region_buffer![1, 2, 3];

        let _a = foo.get_mut(2);

        foo.truncate(2);
    }
}
