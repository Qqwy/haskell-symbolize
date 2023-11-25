# Changelog for `symbolize`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

- Switch from `HashMap ShortText (Weak Symbol)` to `Map ShortText (Weak Symbol)` for the textTosymbol part of the global symbol table. Potentially slightly slower, but HashDoS-resistant. 
 (Note that the `symbolToText :: HashMap Word -> ShortText` is unaffected as its keys are not user-created and guaranteed unique.)
- Remove NOINLINE for lookup as it is now a proper IO function.

## 0.1.0.1 / 0.1.0.2 - 2023-11-24
Fixes in the README and package description only, for better rendering on Hackage.

## 0.1.0.0 - 2023-11-24
Initial version
