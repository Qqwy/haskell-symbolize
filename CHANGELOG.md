# Changelog for `symbolize`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

- Only calculate the SipHash hash of an incoming text _once_ when inserting. (c.f. [#8](https://github.com/Qqwy/haskell-symbolize/issues/8))

## 1.0.2.3

- Swaps the internal usage of lists in the global symbol table for `NonEmpty` lists, since they will never be empty. (PR [#5](https://github.com/Qqwy/haskell-symbolize/pull/5)). Thank you, @Bodigrim!

## 1.0.2.2

- Fixing some typos in the documentation (PR [#3](https://github.com/Qqwy/haskell-symbolize/pull/3)). Thank you, @Bodigrim!

## 1.0.2.1

- Widen dependency bounds, making the package work with the latest version of `containers` and `hashable`.

## 1.0.2.0

- Adds `Textual.toShortTextUnsafe` and related `internUnsafe` and `internUnsafe#`; these skip UTF-8 validity checks. Very useful when working with trusted serialized data.
  - rename `intern##` to `internUnsafe##` (the old name is marked as deprecated.)
- Cleans up `mkWeakSymbol` to be slightly less sketchy in the presence of `accursedUnutterablePerformIO`; thanks @fatho!

## 1.0.1.0

- Add `Data.Data` instance
- Add `Data.Binary` instance

## 1.0.0.4

- Minor documentation improvements

## 1.0.0.3

- Minor documentation improvements

## 1.0.0.2

- Minor documentation improvements

## 1.0.0.1

- Minor documentation improvements

## 1.0.0.0 - 2025-02-17

Completely overhauled implementation:
- The old implementation used `newtype Symbol = Symbol Word`, adding Weak-pointers to this `Word`.
  - This was brittle, since those `Word`s would often get inlined, potentially triggering weak-pointer finalization (too) early.
  - The symbol table had to keep track of mappings in both directions.
  - Int also meant that `unintern` required access to the symbol table.
- The new implementation uses `newtype Symbol# = Symbol# ByteArray#`, and adds weak pointers to this unlifted `ByteArray#`.
  - Much safer, this is how `mkWeak` is intended to be used.
  - The symbol table now only needs to store 'TextHash -> Weak Symbol', and is essentially just a 'weak hashset'. Less than half the memory usage!
  - Much faster `unintern`, as it no longer needs access to the symbol table but is a simple pointer dereference.

## 0.1.0.1 / 0.1.0.2 - 2023-11-24
Fixes in the README and package description only, for better rendering on Hackage.

## 0.1.0.0 - 2023-11-24
Initial version
