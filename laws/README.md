# Laws for Collections-Laws

The code in this project defines the laws that collections must pass.  This file describes the logic behind how these laws are organized and generated.

## What is a `Law`?

A `Law` contains a test that should be true for many collections.  At its core it is a piece of code that returns a `Boolean`--typically an equality--that should hold for any (valid) collection, operation, and/or numeric value passed in to it.

The code is simply text that will form the body of a `run` method of a generated test (see below for information on what a `Test` is).  Because it is a method within a generated class, a number of variables are available for use, including those corresponding to a collection of some type, an element of the same type, and various numbers that one might use in calling methods of that collection (e.g. `take`).

A `Law` is intended to apply broadly but perhaps not universally to all collections.  The running of a `Law` is restricted in two ways.

1. By availability of method.  Within a test, any method that is quoted in back-ticks is checked before generation to ensure that it is actually available on the collection in question.  If one or more methods are unavailable, the test is not run.  If the test is designed to ensure that a method is available on a collection, then the method should be used unquoted!  Note also that extension methods cannot be detected by the run-time reflection used to validate method availability.

2. By flags and/or more intricate testing.  Each collection type can define a number of text tags.  Each law can specify a subset of tags that must be present, and/or a subset that must not be present, in order for the test to be valid.  Additionally, a law can query additional features of the particular test and operation selected, such as whether a binary operation is expected to be associative and symmetric, or not.

Refinement of laws is therefore a two-step process.  When a method is not available, code generation for the combination of that collection and that law will not proceed.  Furthermore, text flags are a property only of collection type and will prevent code generation.  If methods are present and text flags pass, then a test class will be generated.

In the second set of the process, tests will be run only for those parameters for which all additional tests pass.

Thus, availability of back-quoted methods and fulfillment of text tags should be used to ensure that a test can compile.  (Negative tests are not supported--the point of this is to test laws that specify run-time behavior.)  Additional exclusion of inappropriate combinations of particular operations and collections can be achieved by filtering on the `TestInfo` trait before running.

## The fundamental unit of work: `Test`

A `Test` is a class that runs a single test; it is parameterized by collection type and element type, and takes as arguments a particular instance of the collection to be tested, a particular set of operations to apply (if called for by the test), and a particular set of numeric arguments.  It also contains code to run the test corresponding to a particular law.

The actual instances of tests are produced by code generators that assemble the four components of a test: `Instance`, `Ops`, `Numbers`, and `Law`.

## Generating diversity with `Explore` and `Exploratory`

## Instance

## Ops

## Numbers

## Laws
