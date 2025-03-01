# Symbolize
[![Hackage](http://img.shields.io/hackage/v/symbolize.svg)](https://hackage.haskell.org/package/symbolize)
[![HackageDocumentation](https://img.shields.io/badge/documentation-available-blue)](https://hackage.haskell.org/package/symbolize/docs/Symbolize.html)
[![test](https://github.com/Qqwy/haskell-symbolize/actions/workflows/test.yaml/badge.svg?branch=main)](https://github.com/Qqwy/haskell-symbolize/actions/workflows/test.yaml)

Haskell library implementing a global Symbol Table, with garbage collection.

[API Documentation](https://hackage.haskell.org/package/symbolize/docs/Symbolize.html)

  
Symbols, also known as Atoms or Interned Strings, are a common technique
to reduce memory usage and improve performance when using many small strings:

A Symbol represents a string (any `Textual`, so String, Text, ShortText, ByteString, ShortByteString, etc.)

Just like `ShortText`, `ShortByteString` and `ByteArray`, a `Symbol` has an optimized memory representation,
directly wrapping a primitive `ByteArray#`.

Furthermore, a global symbol table keeps track of which values currently exist, ensuring we always deduplicate symbols.
This therefore allows us to:
- Check for equality between symbols in constant-time (using pointer equality)
- Calculate the hash in constant-time (using `StableName`)
- Keep the memory footprint of repeatedly-seen strings low.

This is very useful if you're frequently comparing strings
and the same strings might come up many times.
It also makes Symbol a great candidate for a key in e.g. a `HashMap` or `HashSet`.

The global symbol table is implemented using weak pointers,
which means that unused symbols will be garbage collected.
As such, you do not need to be concerned about memory leaks
(as is the case with many other symbol table implementations).

Symbols are considered 'the same' regardless of whether they originate
from a `String`, (lazy or strict, normal or short) `Data.Text`, (lazy or strict, normal or short) `Data.ByteString` etc.

The main advantages of Symbolize over other symbol table implementations are:

- Garbage collection: Symbols which are no longer used are automatically cleaned up.
- Support for any `Textual` type, including `String`, (strict and lazy) `Data.Text`, (strict and lazy) `Data.ByteString`, `ShortText`, `ShortByteString`, etc.
- Great memory usage:
    - `Symbol`s are simply a (lifted) wrapper around a `ByteArray#`, which is nicely unpacked by GHC.
    - The symbol table is an `IntMap` that contains weak pointers to these same `ByteArray#`s and their associated `StableName#`s
- Great performance:
    - `unintern` is a simple pointer-dereference
    - calls to `lookup` are free of atomic memory barriers (and never have to wait on a concurrent thread running `intern`)
- Thread-safe

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
GlobalSymbolTable { size = 5, symbols = ["Brie","Camembert","Roquefort","hello","world"] }
```
