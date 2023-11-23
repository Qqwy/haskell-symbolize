# Symbolize

Haskell library implementing a global Symbol Table, with garbage collection.

Symbols, also known as Atoms or Interned Strings, are a common technique
to reduce memory usage and improve performance when using many small strings.

By storing a single copy of each encountered string in a global table and giving out indexes to that table,
it is possible to compare strings for equality in constant time, instead of linear (in string size) time.

The main advantages of Symbolize over other Haskell symbol table implementations are:

- Garbage collection: Symbols which are no longer used are automatically cleaned up.
- `Symbol`s have a memory footprint of exactly 1 `Word` and are nicely unpacked by GHC.
- Support for any `Textual` type, including `String`, (strict and lazy) `Data.Text`, (strict and lazy) `Data.ByteString` etc.
- Thread-safe.
- Calls to `lookup` and `unintern` are free of atomic memory barriers (and never have to wait on a concurrent thread running `intern`)
- Support for a maximum of 2^64 symbols at the same time (you'll probably run out of memory before that point).

## Basic usage

This module is intended to be imported qualified, e.g.

```
import Symbolize (Symbol)
import qualified Symbolize
```



```

```