scala-collections-laws
======================

Partially automatic generation of tests for the Scala collections library.  The 
goal is to have a quasi-comprehensive set of collections tests that will catch
regressions in basic functionality in any collection.

These tests are expressed as a series of "laws": one-liners that express relationships
between collections operations that should be true.

These laws are then applied to a set of collection variants which are specified by
a simple of textual replacement macro language.

## Quick start

Clone the repository and run

```bash
sbt -mem 6144 "tests/runMain tests.generated.collection.Test_All
```

If all goes well, the build process will do some compilation and code generation
in project `laws`, then more compilation in project `init`, followed by more
code generation and a test coverage report looking something like this:

```
Created tests for 101 conditions.  Method coverage:
Array_Int_ (NEW); missed 13:
    array          copyToArray    deep           elemManifest   elemTag
    flatten        newBuilder     parCombiner    reversed
    sliceWithKnownBound           sliceWithKnownDelta           thisCollection
    toCollection
collection_IndexedSeq_Int_ (NEW); complete
collection_IterableView_Int_Iterable_Int__ (old); complete
...
collection_mutable_WrappedArray_Int_ (NEW); missed 4:
    array          deep           elemManifest   elemTag
```

Then it will continue on to compile the created tests in project `tests`
(one file per collection variant, plus the Test_All file) and run them.
Compilation typically takes on the order of 15 minutes and the tests
execute in about two minutes.

Again, if all goes well it will conclude with a zero exit code and report some
trivial summary statistics:

```
Test lines 185 were not used.
101 collections run against suite of 348 tests (17416 total)
```

Congratuations!  Your collections were quasi-comprehensively tested.

If you prefer to separate the compilation and test-running phases, just run

```bash
sbt -mem 4096 tests/compile
```

first.  This will compile the `laws` and `init` projects as necessary, and call
their code generation routines.  Note that generated files are only rewritten
when the contents change.

## Common tasks

### Catching a regression

Catching a regression typically does not require deep knowledge of the workings
of scala-collections-laws.  However, you will need to tell sbt which copy of
Scala you want to test.  In `build.sbt` in the root directory of the project,
add a line similar to

```scala
scalaHome in ThisBuild := Some(file("/usr/local/share/scala/2.11/repo/build/pack"))
```

assuming that you've got your copy of the Scala repository in `/usr/local/share/scala/2.11/repo`.

The `scalaVersion` line should be set to the most recent version of Scala if that version is
binary compatible.  If not, the line can be whatever you want, but you need to remove
the `libraryDependencies` line from `laws/build.sbt` and instead drop a compatible
`scala-reflect.jar` in the `laws/lib` (as a SBT unmanaged dependency).


#### Regression class 1: failed assertion.

Test files contain methods that try different combinations of inputs to laws
that require the same inputs.  If a single assertion fails in one of these
methods, the testing in that method terminates.  If very many failures exist,
this means that problems must be repaired one at a time before later ones are
revealed.  However, with regressions the problem is typically more shallow.

Suppose we were working on `Range.scala` and provided this definition for `splitAt`:

```scala
  final override def splitAt(n: Int) = (take(n), drop(n+1))
```

If we run this through the collections tests, we should get output similar to the following:

```
1 collections failed assertions:
  collection_immutable_Range on lines 323 159 270
Details follow.
/=========
| Test line 323 with a = -70; n = 0; x = Range(0, 1, 2, 3) in group 1
|   tests.generated.collection.Test_collection_immutable_Range$$anonfun$test_a_n_x$1$$anonfun$apply$mcVI$sp$5.apply$mcVI$sp(Test_collection_immutable_Range.scala:86)
|   tests.generated.collection.Test_collection_immutable_Range$$anonfun$test_a_n_x$1$$anonfun$apply$mcVI$sp$5.apply(Test_collection_immutable_Range.scala:79)
| ...
| 
| Test line 159 with m = -1; n = 0; x = Range(0, 1, 2, 3); y = Range(4, 5, 6, 7, 8) in group 1
|   tests.generated.collection.Test_collection_immutable_Range$$anonfun$test_m_n_x_y$1$$anonfun$apply$mcVI$sp$41$$anonfun$apply$mcVI$sp$129.apply(Test_collection_immutable_Range.scala:545)
|   tests.generated.collection.Test_collection_immutable_Range$$anonfun$test_m_n_x_y$1$$anonfun$apply$mcVI$sp$41$$anonfun$apply$mcVI$sp$129.apply(Test_collection_immutable_Range.scala:540)
| ...
| 
| Test line 270 with n = 0; x = Range(0, 1, 2, 3) in group 1
|   tests.generated.collection.Test_collection_immutable_Range$$anonfun$test_n_x$1.apply$mcVI$sp(Test_collection_immutable_Range.scala:610)
|   tests.generated.collection.Test_collection_immutable_Range$$anonfun$test_n_x$1.apply(Test_collection_immutable_Range.scala:597)
| ...
\=========
```

The error message tells us that something is wrong with the tests for `collection.immutable.Range`
(more on this later) and gives us an abbreviated stack 

We then go to the `single-line.tests` file, located at `laws/src/main/resources/single-line.tests` and find
that the three lines in question are:

```scala
/* 159 */ y n m !SI8474 ... (n <= x.`size`) implies (x.`patch`(n, y, m).`drop`((0 max n)+y.`size`) theSameAs x.`drop`((0 max n)+(0 max m)))
/* 270 */ n !SI8815 ... val (x1,x2) = x.`splitAt`(n); (x1 theSameAs x.`take`(n)) && (x2 theSameAs x.`drop`(n))
/* 323 */ n a !M ... n < 0 || n >= x.`size` || { x.`updated`(n,a) theSameAs (x.`take`(n).`:+`(a).`++`(x.`drop`(n+1))) }
```

Of these three, line 270 seems the simplest to verify.  We go to the REPL, copying
the values for `n` and `x` listed in the error message and start building up the test line by hand:

```scala
scala> val n = 0
n: Int = 0

scala> val x = 0 to 3
x: scala.collection.immutable.Range.Inclusive = Range(0, 1, 2, 3)

scala> x.splitAt(0)
res0: (scala.collection.immutable.Range, scala.collection.immutable.Range) = (Range(),Range(1, 2, 3))
```

And here we see what is wrong: the `take` range is correct, but the `drop` one is missing the first element.


#### Regression class 2: runtime exception

The test framework tries very hard (harder than `scala.util.Try`) to catch runtime exceptions.
However, it does not possess information about the precise location of the exception; this leads
to a little extra manual work.

Suppose after reverting the change to `Range.scala`, we decide that it doesn't make sense
to split negative elements:

```scala
  final override def splitAt(n: Int) =
    if (n < 0) throw new InvalidArgumentException("Negative in split")
    else (take(n), drop(n))
```

We then promptly forget we did this and start running collections tests.

```
1 collections crashed during tests with an unanticipated exception:
  collection_immutable_Range
Details follow.
/=========
| Negative in split
|   scala.collection.immutable.Range.splitAt(Range.scala:287)
|   scala.collection.SeqLike$class.patch(SeqLike.scala:504)
|   scala.collection.AbstractSeq.patch(Seq.scala:41)
|   tests.generated.collection.Test_collection_immutable_Range$$anonfun$test_m_n_x_y$1$$anonfun$apply$mcVI$sp$41$$anonfun$apply$mcVI$sp$129$$anonfun$apply$5.apply$mcZ$sp(Test_collection_immutable_Range.scala:543)
  (lots more lines here)
|   sbt.Logger$$anon$4.apply(Logger.scala:90)
|   sbt.TrapExit$App.run(TrapExit.scala:244)
|   java.lang.Thread.run(Thread.java:662)
\=========


1 collections failed (1 with crashes, 0 with failed assertions)
Test lines 185 were not used.
100 collections run against suite of 348 tests (17395 total)
java.lang.RuntimeException: Nonzero exit code: 1
        at scala.sys.package$.error(package.scala:27)
```

Since we get the stack trace, we can see exactly where the exception arose in
our code: it's in `splitAt`.  But what was the test that triggered this problem?
The very long line above ends with `Test_collection_immutable_Range.scala:543`.
If we go look in this file (it's at `tests/target/scala-2.11/src_managed/main`)
we find the following at line 543:

```
if (!{(n <= x.size) implies (x.patch(n, y, m).take(n) theSameAs x.take(n))}) { throw new AssertionError("Test line 157 with "+message) }
```

which informs us that the test line was 157 in `single-line.tests`.  There is
no indication of what combination of variables was used; typically the stack
trace plus the law line is plenty to figure that out.

From this we surmise that somewhere in `patch` there is a call to `splitAt`, and it
fails on negative indices in a bad way.  This is unacceptable, so we again revert
the change to `splitAt`.


#### Regression class 3: compilation error

The presumption is that compilation errors will be rare.  If a change has caused
a compilation error, you will have to work through SBT's error reporting facilities
and look at the generated code to figure out what went wrong.  Since each line of
code specifies the line of the law which generated it, you can quickly get back
to the test itself if you need to.

This procedure is documented below in the "Creating a new law" section.



### Annotating or removing collections bugs

The collections tests will not pass successfully if even a single error is found.
This requires the entire test-suite to avoid any existing bugs in the source code.

The convention for bugs is to place the issue number in the `flags` line of the
affected collection(s) in the format `SI1234` (no dash) in the file `replacements.tests`
in `laws/src/main/resources`.

TODO: add more content.


### Adding or altering laws

Because generated code contains the line numbers of the laws in `single-line.tests`,
any change that alters the line numbers will result in a lot of recompilation.  If
you're trying to go through test cycles quickly, try to get the file into the right
shape first, then work on content.

TODO: add more content.

#### Altering an existing laws

TODO: this section

#### Adding a new law

TODO: this section



### Adding or altering collections

TODO: This section