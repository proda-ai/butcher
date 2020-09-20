# Revision history for butcher

## 2.0.0.0  -- Sept 2020

Large internal refactor including some breaking API changes.

- Add the "Applicative" interface in addition to the existing "Monadic" one.
  This is slightly less expressive but conceptually cleaner/safer (and its
  implementation is nicer). For best readability you may need `ApplicativeDo`.
- The applicative interface is *NOT* finished and the test-suite does not
  cover it.
- Add the `traverseBarbie` construct to elegantly define a parser for a
  config data-structure.\
  Introduces a dependency on the `barbies` library.
- Refactor the module structure a bit, and change the API of the central
  `runCmdParser` function. It now returns a `PartialParseInfo`. Essentially,
  `runCmdParser` is a combination of the previous `runCmdParser` and the
  previous `simpleCompletion`. This API design is a curious advantage to
  laziness: Returning a complex struct is harmless as fields that the user
  does not use won't be evaluated. The downside is that the core function now
  looks like a complex beast, but the upside is that there is no need to
  expose multiple functions that are supposed to be chained in a certain way
  to get all functionality (if desired), and we still _can_ provide simpler
  versions that are just projections on the `PartialParseInfo`.
- Remove deprecated functions
- `peekCmdDesc` is now guaranteed to yield the proper full `CmdDesc` value
  for the current command or child-command.
- Remove the `mainFromCmdParserWithHelpDesc` function, because it is redundant
  given the new semantics of `peekCmdDesc`.
- Stop support for an anti-feature: The implicit merging of multiple
  sub-commands definitions with the same name.
- Internal refactor: The monadic interface now uses two-phase setup: First step
  is to create a full CommandDesc value, second is running the parser on input
  while the CommandDesc is chained along

## 1.3.3.2  -- June 2020

* Support ghc-8.10
* Drop support for ghc < 8.4
* Fix a somewhat hidden issue in the cabal file
* Add support for building / testing via haskell.nix nixpkgs overlay

## 1.3.3.1  -- April 2020

* Fix a cabal file mistake

## 1.3.3.0  -- April 2020

* Fix bug with params with default when parsing from commandline
* Add the `descendDescTo` function

## 1.3.2.3  -- June 2019

* Fix broken build when using deque>=0.3

## 1.3.2.2  -- June 2019 (broken, disabled on hackage)

* Fix too-shallow descriptions on siblings for partial parses returned
  for interactive usage

## 1.3.2.1  -- October 2018

* Adapt/Use latest version of `deque` to fix ghc-8.6 problems

## 1.3.2.0  -- October 2018

* Fix for simpleCompletion
* Expose some bindings that were forgotten in previous release
* Bounds fixed for ghc-8.6 (also via revision in 1.3.1.1)

## 1.3.1.1  -- April 2018

* Fixup version bound

## 1.3.1.0  -- April 2018

* Add/Expose two more functions: addAlternatives and varPartDesc

## 1.3.0.1  -- April 2018

* Support ghc-8.4
* Drop support for ghc<8

## 1.3.0.0  -- February 2018

* Experimental: Hidden commandparts (do not appear in help)
* Experimental: Bash completion
* Add addHelpCommandWith to support user-defined column count
* Fix help document printing (ribbons)
* Fix completion behaviour

## 1.2.1.0  -- November 2017

* Fix bug in 'ppUsageWithHelp'
* some utilities for interactive usage in new module
  `UI.Butcher.Monadic.Interactive`

## 1.2.0.0  -- October 2017

* Rename some `Monadic.Param.*`, deprecate old versions.
    - `addReadParam` -> `addParamRead`
    - `addReadParamOpt` -> `addParamReadOpt`
    - `addStringParam` -> `addParamString`
    - `addStringParamOpt` -> `addParamStringOpt`
    - `addStringParams` -> `addParamStrings`
    - `addRestOfInputStringParam` -> `addParamRestOfInput`
* Add functions `addParamNoFlagString`, `addParamNoFlagStringOpt`,
  `addParamNoFlagStrings`
* Fix flag parsing behaviour (ignore initial spaces)

## 1.1.1.0  -- October 2017

* Add `addNullCmd` function that descends into childcommand on an epsilon match
* Add `addStringParams` function that reads all remaining words

## 1.1.0.2  -- September 2017

* Improve 'usage' pretty-printing

## 1.1.0.1  -- August 2017

* Adapt for ghc-8.2

## 1.1.0.0  -- May 2017

* First version. Released on an unsuspecting world.
