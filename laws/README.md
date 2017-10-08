# Laws for Collections-Laws

The code in this project defines the laws that collections must pass.  This file describes the logic behind how these laws are organized and generated.

## What is a `Law`?

A `Law` contains a test that should be true for many collections (see `Laws.scala`).  At its core it is a piece of code that returns a `Boolean`--typically an equality--that should hold for any valid collection, operation, and/or numeric value passed in to it.

The code is simply text that will form the body of a `run` method of a generated test (see below for information on what a `Test` is).

A `Law` is intended to apply broadly but perhaps not universally to all collections.  The running of a `Law` is restricted in two ways.

1. By availability of method.  Within a test, any method that is quoted in back-ticks is checked before generation to ensure that it is actually available on the collection in question.  If one or more methods are unavailable, the test is not run.  If the test is designed to ensure that a method is available on a collection, then the method should be used unquoted!  Note also that extension methods cannot be detected by the run-time reflection used to validate method availability.

2. By flags and/or more intricate testing.  Each collection type can define a number labels that are instances of the `Tag` class.  Each law can specify a subset of tags that must be present, and/or a subset that must not be present, in order for the test to be valid.  Additionally, a law can query additional features of the particular test and operation selected, such as whether a binary operation is expected to be associative and symmetric, or not.

Refinement of laws is therefore a two-step process.  When a method is not available, code generation for the combination of that collection and that law will not proceed.  Furthermore, tags are a property only of collection type and will prevent code generation.  If methods are present and text flags pass, then a test class will be generated.

In the second set of the process, tests will be run only for those parameters for which all additional tests pass.

Thus, availability of back-quoted methods and fulfillment of text tags should be used to ensure that a test can compile.  (Negative tests are not supported--the point of this is to test laws that specify run-time behavior.)  Additional exclusion of inappropriate combinations of particular operations and collections can be achieved by filtering on the `TestInfo` trait before running.

The individual laws are specified inside the `Laws` object.  In addition to extension methods on strings that assist in constructing a `Law` object, `Law` itself allows tags to be added using the `and`, `not`, and `filter` methods (the last of which takes a predicate that checks the `TestInfo` for that test at runtime).

The `sourcecode` package is used to enable informative error reporting by capturing line numbers and file names of various components of laws, most notably the line where the text is declared in the `Laws` object.

### Available variables

The variables available to code in laws are those in the `Test` class (see below for details about this class).  While _all_ methods of `Test` are technically available (including those code-generated in the leaf classes that inherit from `Test` and contain the literal code), the following are intended for use:

#### Elements and collections

* `x` is the primary collection being tested.  Its length is `xsize`.
* `y` is an auxillary collection of the same type (used for operations like `++`).  Its length is `ysize`.
* `a` is an element of the same type as stored in `x`
* `b` is an element of a different type (the same type as produced from `a` by the function `g` - see below)
* `zero` is an element of the type stored in `x` which is a zero with respect to the function `op` - see below); call `hasZero` to see if it exists

#### Functions, predicates, and binary operations

* `f` is a function that converts elements of `x` into other elements of the same type
* `g` is a function that converts elements of `x` into elements of some different type (the type of `b`).
* `p` is a predicate that returns true or false for elements of `x`
* `pf` is a partial function that will where defined convert elements of `x` into other elements of the same type
* `op` is a binary operation that will take two elements of `x` and produce one of the same type; if the operation is associative and symmetric (like addition), `isSymOp` will return true

#### Numbers

* `n` is a number between 0 and the length of `x`
* `nn` is a non-negative number (may be bigger than `x`)
* `m` is a number between 0 and the length of `y`
* `mm` is another non-negative number (may be bigger than `y`)
* `r` is a number that may be positive or negative


### Available methods

In addition to the normal methods availble in Scala code, the extension methods provided by `Tests.ComparesTo` and `Tests.Logic` are imported into the scope of the test methods.

#### Comparison methods

* `theSameAs` checks whether two things that can at least be implicitly converted to a `TraversableOnce` have the same elements in the same order.  For example, `Iterator('c', 'o', 'd') theSameAs "cod"` would return true.

* `correspondsTo` checks whether the two sides have the same elements as each other, the same number of times; order is not important.  For example, `"bass" correspondsTo Array('s', 'b', 's', a')` would return true.

* `isPartOf` checks whether every element in the left-hand side also exists in the right (with the right having at least as many duplicates if there are duplicates).  For example, `Vector('n', 'n', 'm') isPartOf "minnow"` would return true.

#### Logical methods

* `implies` is a short-circuiting method that returns false if the left-hand side is true but the right is false; it otherwise returns true (as is the case with logical implication).  The left-hand side is evaluated first.

* `impliedBy` is a short-circuiting method that returns false if the right-hand side is true but the left is false; it otherwise returns true (as is the case with the logical reverse implication).  The left-hand side is evaluated first, which makes it potentially have different sid effects than `implies` with the order of arguments flipped.

## The fundamental unit of work: `Test`

A `Test` is a class that runs a single test; it is parameterized by collection type and element type, and takes as arguments a particular instance of the collection to be tested, a particular set of operations to apply (if called for by the test), and a particular set of numeric arguments.

The leaf `Test` classes are produced via code generation and contain code to run each valid law, as well as to select a single test to run.  Because the run methods within a generated class, a number of variables are available for use, including those corresponding to a collection of the appropriate type, an element of the same type, and various numbers that one might use in calling methods of that collection (e.g. `take`).

The actual instances of tests are produced by code generators that assemble the four components of a test: `Instance`, `Ops`, `Numbers`, and `Law`.

### Instance

### Ops

### Numbers

## Generating Test Code with Companions

## Running tests with Runner

### Generating diversity with `Explore` and `Exploratory`
