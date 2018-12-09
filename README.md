# Region Buffer
[![Linux build status](https://img.shields.io/travis/Aaronepower/region_buffer.svg?branch=master)](https://travis-ci.org/Aaronepower/region_buffer)
[![](https://img.shields.io/crates/d/region_buffer.svg)](https://crates.io/crates/region_buffer)
[![](https://img.shields.io/github/issues-raw/Aaronepower/region_buffer.svg)](https://github.com/Aaronepower/region_buffer/issues)
[![](https://tokei.rs/b1/github/Aaronepower/region_buffer?category=code)](https://github.com/Aaronepower/region_buffer)
[![Documentation](https://docs.rs/region_buffer/badge.svg)](https://docs.rs/region_buffer/)
[![Donate using Liberapay](https://liberapay.com/assets/widgets/donate.svg)](https://liberapay.com/Aaronepower/donate)

A growable array allowing for multiple mutable non overlapping regions from
the same `Vec`.

```
 use region_buffer::RegionBuffer;
let mut buffer = RegionBuffer::new();

buffer.push(1);
buffer.push(2);

assert_eq!(buffer.len(), 2);

let mut a = buffer.get_mut(0);
let mut b = buffer.get_mut(1);

assert_eq!(*a, 1);
assert_eq!(*b, 2);

*a = *b;
*b = 3;

assert_eq!(*a, 2);
assert_eq!(*b, 3);
```
There is a `region_buffer` macro provided to make initialisation more
convenient.
```
 #[macro_use]
 extern crate region_buffer;

 fn main() {
let strings = region_buffer!["Hello", "World", "!"];

let mut greeting =  strings.get_mut(0);
let mut noun =  strings.get_mut(1);
let mut punctuation =  strings.get_mut(2);

*greeting = "Hallo";
*noun = "Peter";
*punctuation = ".";

let string = format!("{} {}{}", greeting, noun, punctuation);
assert_eq!(string, "Hallo Peter.")
 }
```
The macro can also be used to specify and initialise large regions of
memory.
```
 #[macro_use]
 extern crate region_buffer;

 type Slice<'a> = region_buffer::Slice<'a, u8>;

 struct Console;
 impl Console {
   fn new(_: Slice, _: Slice, _: Slice, _: Slice) {}
 }

 fn main() {
let memory = region_buffer![0; 0xFFFF];

let rom =  memory.region(0, 0x800);
let gpu = memory.region(0x800, 0x1600);
let sound = memory.region(0x1600, 0x2400);
let ram = memory.region(0x2400, 0xFFFF);

let console = Console::new(rom, gpu, sound, ram);
 }
```
