# \[@@deriving binary_bytes\] [![OCaml](https://github.com/akabe/ppx_deriving_binary_bytes/actions/workflows/ocaml.yaml/badge.svg)](https://github.com/akabe/ppx_deriving_binary_bytes/actions/workflows/ocaml.yaml)

An OCaml PPX extension for type-driven generation of binary serializers and deserializers

`ppx_deriving_binary_bytes` is a PPX syntax extension that generates binary serializers and
deserializers from an OCaml type definitions.

[The online API documentation is available](https://akabe.github.io/ppx_deriving_binary_bytes/index.html).

## Basic usage

`ppx_deriving_binary_bytes` is triggered by `[@@deriving binary_bytes]` annotation
attached to a type declaration.

``` ocaml
# #require "ppx_deriving_binary_bytes, ppx_deriving_binary_bytes.runtime" ;;
# open Ppx_deriving_binary_bytes_runtime.Std ;;
# type ty = ... [@deriving binary_bytes] ;;
val ty_of_binary_bytes : bytes -> int -> ty * int
val binary_bytes_of_ty : BytesBuffer.t -> ty -> unit
```

The deserializer `ty_of_binary_bytes` takes a `bytes` object and a position to
start conversion, and returns a value of type `ty` and a position at the end of
parsing.
The serializer `binary_bytes_of_ty` appends a binary-encoded value of type `ty` to
the end of a given buffer.
It is possible to generate serialization functions by using
`[@@deriving of_binary_bytes]`, or deserializers by `[@@deriving binary_bytes_of]`.

`[%of_binary_bytes: (type-expression)]` is an expression decodes a `bytes`
into a value of `(type-expression)`.

``` ocaml
# let cs = Bytes.of_string "\x01\x00\x00\x00\x0a\x00" in
  [%of_binary_bytes: uint32le * uint16le] cs 0 ;;
- : (uint32 * uint16) * int = ((1l, 10), 6)
```

In addition, `[%binary_bytes_of: (type-expression)]` is an expression appends
a value of `(type-expression)` to a buffer.

``` ocaml
# let bb = BytesBuffer.create 10 ;;
# [%to_binary_bytes: uint32le * uint16le] bb (1l, 10) ;;
- : unit = ()
# Format.printf "%a@." Printer.pp_string_hex (BytesBuffer.contents bb) ;;
01 00 00 00 0a 00
```

## Basic types

### Integers and floating-point values

| Type in `ppx_deriving_binary_bytes` | OCaml type | Size (bytes) | Endian | Description |
|:--|:--|:--|:--|:--|
| `char` | `char` | 1 | N/A |
| `int8` | `int` | 1 | N/A |
| `uint8` | `int` | 1 | N/A |
| `int16be` | `int` | 2 | big |
| `int16le` | `int` | 2 | little |
| `uint16be` | `int` | 2 | big |
| `uint16le` | `int` | 2 | little |
| `int32be` | `int32` | 4 | big |
| `int32le` | `int32` | 4 | little |
| `int32bei` | `int` | 4 | big | An int-version of `uint32be` |
| `int32lei` | `int` | 4 | little | An int-version of `uint32le` |
| `int64be` | `int64` | 8 | big |
| `int64le` | `int64` | 8 | little |
| `int64bei` | `int` | 8 | big | An int-version of `uint64be` |
| `int64lei` | `int` | 8 | little | An int-version of `uint64le` |
| `float32be` | `float` | 4 | big | IEEE754 32-bit floating point |
| `float32le` | `float` | 4 | little | IEEE754 32-bit floating point |
| `float64be` | `float` | 8 | big | IEEE754 64-bit floating point |
| `float64le` | `float` | 8 | little | IEEE754 64-bit floating point |

### Fixed-length formats

| Type in `ppx_deriving_binary` | OCaml type| Description |
|:--|:--|:--|
| `(string [@length N])` | `string` | Fixed-length strings of `N` bytes |
| `(bytes [@length N])` | `bytes` | Fixed-length bytes of `N` bytes |
| `('a list [@length N])` | `'a list` | Fixed-length lists of `N` elements |
| `('a array [@length N])` | `'a array` | Fixed-length arrays of `N` elements |

### Variable-length formats

| Type in `ppx_deriving_binary` | OCaml type| Description |
|:--|:--|:--|
| `CString.t` | `string` | Null-terminated strings |
| `GreedyString.t` | `string` | Strings greedily read until the end of input |
| `'a PrefixedString.t` | `string` | Strings that have prefix of type `'a` |
| `'a GreedyList.t` | `'a list` | Lists greedily read until the end of input |
| `('a, 'b) PrefixedList.t` | `'a list` | Lists that have prefix of type `'a` |

The prefixed formats have a _prefix_, the number of elements in trailing data, at the head of data.
For example, `"Hello"` of type `uint16le PrefixedString.t` is represented as follows:

```
HEX       05  00  48  65  6c  6c  6f
DATA      0x0050  H   e   l   l   o
SCHEMA    prefix  contents (len=5)
```

## Tuples

A tuple type `(t1 * t2 * ... * tn)` is represented as a binary sequence of
`t1`, `t2`, ..., and `tn`.
For example, `("ABCD", 0x1234, 0x5678)` of type
`((string [@length 4]) * uint16le * uint16le)` is
encoded as:

```
HEX       65  66  67  68    34  12    78  56
DATA      A   B   C   D     0x1234    0x5678
SCHEMA    string            uint16le  uint16le
```

## Records

The following record has the same schema as the above tuple:

``` ocaml
type t = {
  format : (string [@length 4]);
  width  : uint16le;
  height : uint16le;
} [@@deriving binary_bytes]
```

Records support more rich functionality than tuples:
during parsing a record, you can use values of fields that have already read,
in `[@of_binary_bytes fun ~field1 ~field2 ... cs i -> ...]` annotation.
In the following example, `foo` field is decoded if `foo_exists` is not zero,
or `foo` is `None` otherwise.

``` ocaml
type t = {
  foo_exists : uint8;
  foo : uint8 option
    [@of_binary_bytes fun ~foo_exists cs i ->
       if foo_exists = 0 then (None, i)
       else let x, j = uint8_of_binary cs i in (Some x, j)];
  baz : uint8;
} [@@deriving of_binary_bytes]
```

Note that you must write labelled arguments as the same as a field label **syntactically**.
For instance, `parse_foo` has labelled arguments as follows, but
a value of `foo_exists` is not passed:

``` ocaml
let parse_foo ~foo_exists cs i = ...

type t = {
  foo_exists : uint8;
  foo : uint8 option [@of_binary parse_foo];
  baz : uint8;
} [@@deriving of_binary_bytes]
```

If you want to define a function out of `[@of_binary_bytes]`, eta abstraction
is neccesary like `[@of_binary_bytes fun ~foo_exists -> parse_foo ~foo_exists]`.

## Bitfields

Bitfields are records with annotated with `[@@type: t]`.
The record type

``` ocaml
type t = {
  red   : int [@length 5];
  green : int [@length 6];
  blue  : int [@length 5];
}
[@@type: uint16be]
[@@deriving binary_bytes]
```

is represented as a 16-bit integer like:

```
BIT       | 15 14 13 12 11 | 10  9  8  7  6  5 |  4  3  2  1  0 |
SCHEMA    | blue           | green             | red            |
```

You can explicitly specify a bit position with `[@offset N]`.

A base type must be a type defined as `int` internally.
Please use `int32{be,le}i` or `int64{be,le}i` instead of `int32{be,le}`, `int64{be,le}`
for 32-bit or 64-bit integers.

## Variants

Variants are represented as a pair of a _tag_ and constructor arguments, e.g.,

``` ocaml
type color =
  | No_color                     [@value 0x1111]
  | Gray of uint8                [@value 0x2222]
  | RGB of uint8 * uint8 * uint8 [@value 0x3333]
[@@tag_type: uint16le]
[@@deriving binary_bytes]
```

- `No_color` is `11 11`,
- `Gray 0x42` is `22 22 42`, and
- `RGB (0xaa, 0xbb, 0xcc)` is `33 33 aa bb cc`.

`ppx_deriving_binary_bytes` does not support GADTs.

## Polymorphic variants

Polymorphic variants are almost the same as variants excluding
the difference that you need to use `[@tag_type: t]` instead of `[@@tag_type: t]`.

``` ocaml
type t =
  [
    | `A of uint32le
    | `B of
      [
        | `BA of uint64le
        | `BB of char
      ] [@tag_type: uint32lei]
  ]
[@tag_type: uint16le]
[@@deriving binary_bytes]
```

### ELSE-cases

Variants and polymorphic variants support `[@else]` annotation.

``` ocaml
type t =
  | A
  | B
  | C of uint8 [@else]
[@@tag_type: uint16]
[@@deriving binary_bytes]
```

In the above example, `A` and `B` have tags `0x0000` and `0x0001` respectively, while `C` has **no** tag.
`C` is used when a given bytes cannot be decoded into `A` or `B`.

``` ocaml
# [%of_binary_bytes: t] (Bytes.of_string "\x00") ;;
- : t = A
# [%of_binary_bytes: t] (Bytes.of_string "\x01") ;;
- : t = B
# [%of_binary_bytes: t] (Bytes.of_string "\x02") ;;
- : t = C 2
# [%of_binary_bytes: t] (Bytes.of_string "\xff") ;;
- : t = C 255
```
