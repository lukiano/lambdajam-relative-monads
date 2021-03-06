Using Relative Monads for Cheap Syntax
======================================

Welcome to the YOW LambdaJam workshop on Relative monads.

In this workshop we will work through a simple use case for relative monads and compare and constrast it with
different approaches. We will implement error handling on top of a very simple file system library.
We will first implement error handling specific to the file system library, then generalise the
error handling code using relative monads and, finally, do the same thing using mtl. For those with
time left at the end there are some additional exercises to further explore the concept.

This workshop is available in both Haskell and Scala, please go to the corresponding section for 
your language of choice.

[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/CommBank/lambdajam-relative-monads?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

Haskell
=======

Prerequisite
------------

* GHC (tested on 7.10.1, 7.8.4)
* cabal

Setup
-----

1. Go to the haskell subfolder
2. Run `cabal sandbox init`, `cabal install --dependencies-only`, `cabal configure --enable-tests`, `cabal build`.
3. Run `cabal repl` to enter ghci.
4. Use `:r` to reload and check your changes.
5. To run the tests run `cabal test`.

Exercises
---------

1. Have a look at `src/Result.hs` for the simple error handling type that we are using.
2. **FS** is a very simple file system library that allows you to perform some `IO` actions given a
   current working directory and handles failure by wrapping the errors in `Result`. Implement the
   undefined error handling functions  and `ls` in `src/FS.hs`.
3. Lets now abstract the common error handling functionality using _relative monads_ in particular a
   relative monad to `Result`.
  1. See `src/RelMonad.hs` for the relative monad type class. A _relative monad_ needs:
    * a relative return lifting the monad it is relative to into the target context
    * and, a relative bind allowing functions to map across the monad it is relative to in the
      context of the larger monad. Different formulations of relative bind are possible.
  2. To help us later implement `rMap` and `rFlatMap` in `src/RelMonad.hs`.
  3. Now lets add the error handling. Implement the undefined functions in `src/RelResult.hs`.
  4. To apply it to `FS` implement an instance of `RelMonad` of `FS` relative to `Result` in `src/FS.hs`.
  5. In order to use it implement `rLS` in `src/FS.hs` using the `RelResult` functions instead of
     the `FS` specific functions.
4. Lets now do the same thing using _mtl_.
  1. First we need a `ResultT` so implement the undefined functions in `src/ResultT.hs`.
  2. Next we need error handling so implement the undefined functions in `src/MonadResult.hs`.
  3. To create our mtl version of the file system library implement the undefined functions in `FST.hs`.
  4. In order to use it implement `tLS` in `src/FST.hs` using the `MonadResult` functions.

Extension exercises
-------------------

Extension exercises can be found in `src/Extension.hs`

* Can you implement a monad instance for `R` assuming a relative monad instance relative to `M` and the monad instance for`M`? What about a the same for `MonadPlus`?  What about the identity monad?
* Can you implement a general composition of two relative monad instances, the first `R` to `M` and the second `M` and `N`, to directly relate `R` to `N`? 
* Can you create an instance of _relative monad_ for `IO` relative to `Result`?
* Can you implement `tMap`?
* Implement `rAsk` and `rLocal`
* Can you create an instance of a _relative monad_ for `FS` relative to `Reader FilePath`?
* Can you turn `RelMonad` into another of the `mtl` patterns such as `MonadError`, `MonadReader`, etc
  and use it the same way?  What about patterns using other packages? (E.g. `mmorph`, `layers`.)


Scala
=====

Prerequisite
------------

* Java

Setup
-----

1. Go to the scala subfolder
2. Run `./sbt`. After a while you will get the `sbt` shell.
3. Run `~compile` to get continuous compilation. Press enter to stop.
4. Run `test` to run the tests or `~test` for continuous tests. To test only code for a specific
   module run `test-only *.<ModuleName>Spec` for example `test-only *FSSpec`.
5. Run `console` to get a Scala shell.

Exercises
---------

1. Have a look at `src/main/scala/Result.scala` for the simple error handling type that we are using.
2. **FS** is a very simple file system library that allows you to perform some `IO` actions given a
   current working directory and handles failure by wrapping the errors in `Result`. Implement the
   undefined error handling functions  and `ls` in `src/main/scala/FS.scala`.
3. Lets now abstract the common error handling functionality using _relative monads_ in particular a
   relative monad to `Result`.
  1. See `src/main/scala/RelMonad.scala` for the relative monad type class. A _relative monad_ needs:
    * a relative return lifting the monad it is relative to into the target context
    * and, a relative bind allowing functions to map across the monad it is relative to in the
      context of the larger monad. Different formulations of relative bind are possible.
  2. Now lets add the error handling. Implement the undefined functions in `src/main/scala/RelResult.scala`.
  3. To apply it to `FS` implement an instance of `RelMonad` of `FS` relative to `Result` in `src/main/scala/FS.scala`.
  4. In order to use it implement `rLS` in `src/main/scala/FS.scala` using the `RelResult` functions instead of
     the `FS` specific functions.
4. Lets now do the same thing using the _mtl_ functionality from Scalaz.
  1. First we need a `ResultT` so implement the undefined functions in `src/main/scala/ResultT.scala`.
  2. Next we need error handling so implement the undefined functions in `src/main/scala/MonadResult.scala`.
  3. To create our mtl version of the file system library implement the undefined functions in `FST.scala`.
  4. In order to use it implement `tLS` in `src/main/scala/FST.scala` using the `MonadResult` functions.

Extension exercises
-------------------

Extension exercises can be found in `src/main/scala/Extension.scala`

* Can you implement a monad instance for `R` assuming a relative monad instance relative to `M` and the monad instance for`M`? What about a the same for `MonadPlus`?  What about the identity monad?
* Can you implement a general composition of two relative monad instances, the first `R` to `M` and the second `M` and `N`, to directly relate `R` to `N`?
* Can you create an instance of _relative monad_ for `Future` relative to `Result`?
* Can you implement `tMap`?
* Implement `rAsk` and `rLocal`
* Can you create an instance of a _relative monad_ for `FS` relative to `Reader FilePath`?
* Can you turn `RelMonad` into another of the `mtl` patterns such as `MonadError`, `MonadReader`, etc
  and use it the same way?  What about patterns using other packages?
