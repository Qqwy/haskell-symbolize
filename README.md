# Symbolize
[![Hackage](http://img.shields.io/hackage/v/symbolize.svg)](https://hackage.haskell.org/package/symbolize)
[![HackageDocumentation](https://img.shields.io/static/v1?label=Documentation&message=Available&color=success)](https://hackage.haskell.org/package/symbolize/docs/Symbolize.html)
[![test](https://github.com/Qqwy/haskell-symbolize/actions/workflows/test.yaml/badge.svg?branch=main)](https://github.com/Qqwy/haskell-symbolize/actions/workflows/test.yaml)

Haskell library implementing a global Symbol Table, with garbage collection.

[API Documentation](https://hackage.haskell.org/package/symbolize/docs/Symbolize.html)

Symbols, also known as Atoms or Interned Strings, are a common technique
to reduce memory usage and improve performance when using many small strings.

By storing a single copy of each encountered string in a global table and giving out indexes to that table,
it is possible to compare strings for equality in constant time, instead of linear (in string size) time.

The main advantages of Symbolize over existing symbol table implementations are:

- Garbage collection: Symbols which are no longer used are automatically cleaned up.
- `Symbol`s have a memory footprint of exactly 1 `Word` and are nicely unpacked by GHC.
- Support for any `Textual` type, including `String`, (strict and lazy) `Data.Text`, (strict and lazy) `Data.ByteString` etc.
- Thread-safe.
- Efficient: Calls to `lookup` and `unintern` are free of atomic memory barriers (and never have to wait on a concurrent thread running `intern`)
- Support for a maximum of 2^64 symbols at the same time (you'll probably run out of memory before that point).

## Basic usage

This module is intended to be imported qualified, e.g.


```haskell
import Symbolize (Symbol)
import qualified Symbolize
```

To intern a string, use `intern`:

```haskell
>>> hello = Symbolize.intern "hello"
>>> world = Symbolize.intern "world"
>>> (hello, world)
(Symbolize.intern "hello",Symbolize.intern "world")
```

Interning supports any `Textual` type, so you can also use `Data.Text` or `Data.ByteString` etc.:

```haskell
>>> import Data.Text (Text)
>>> niceCheeses = fmap Symbolize.intern (["Roquefort", "Camembert", "Brie"] :: [Text])
>>> niceCheeses
[Symbolize.intern "Roquefort",Symbolize.intern "Camembert",Symbolize.intern "Brie"]
```

And if you are using OverloadedStrings, you can use the `IsString` instance to intern constants:

```haskell
>>> hello2 = ("hello" :: Symbol)
>>> hello2
Symbolize.intern "hello"
```

Comparisons between symbols run in O(1) time:

```haskell
>>> hello == hello2
True
>>> hello == world
False
```

To get back the textual value of a symbol, use `unintern`:

```haskell
>>> Symbolize.unintern hello
"hello"
```

If you only want to check whether a string is already interned, use `lookup`:

```haskell
>>> Symbolize.lookup "hello"
Just (Symbolize.intern "hello")
```

Symbols make great keys for `Data.HashMap` and `Data.HashSet`.
Hashing them is a no-op and they are guaranteed to be unique:

```haskell
>>> import qualified Data.Hashable as Hashable
>>> Hashable.hash hello
0
>>> fmap Hashable.hash niceCheeses
[2,3,4]
```

For introspection, you can look at how many symbols currently exist:

```haskell
>>> Symbolize.globalSymbolTableSize
5
>>> [unintern (intern (show x)) | x <- [1..5]]
["1","2","3","4","5"]
>>> Symbolize.globalSymbolTableSize
10
```

Unused symbols will be garbage-collected, so you don't have to worry about memory leaks:

```haskell
>>> System.Mem.performGC
>>> Symbolize.globalSymbolTableSize
5
```

For deeper introspection, you can look at the Show instance of the global symbol table:
/(Note that the exact format is subject to change.)/

```haskell
>>> Symbolize.globalSymbolTable
GlobalSymbolTable { count = 5, next = 10, contents = [(0,"hello"),(1,"world"),(2,"Roquefort"),(3,"Camembert"),(4,"Brie")] }
```