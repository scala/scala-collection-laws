scala-collections-laws
======================

Partially automatic generation of tests for the Scala collections library.  The 
goal is to have a quasi-comprehensive set of collections tests that will catch
regressions in basic functionality in any collection.

These tests are expressed as a series of "laws": one-liners that express relationships
between collections operations that should be true.

** EVERYTHING BELOW HERE IS OUT OF DATE **

These laws are then applied to a set of collection variants which are specified by
a simple of textual replacement macro language.

## Latest nontrivial changes

 * Branching macros (choose which replacement based on a flag).
See section "Altering collections in a way that causes many tests to fail".

## Quick start

Clone the repository and run

```bash
sbt -mem 6144 "tests/runMain tests.generated.collection.Test_All"
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
(more on this later) and gives us an abbreviated stack trace.

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



### Dealing with collections bugs

The collections tests will not pass successfully if even a single error is found.
This requires the entire test-suite to avoid any existing bugs in the source code.

The convention for bugs is to place the issue number in the `flags` line of the
affected collection(s) in the format `SI1234` (no dash) in the file `replacements.tests`
in `laws/src/main/resources`.  The offending test is then marked with `!SI1234`.
This indicates to not run the test if the offending flag is present in the collection.

#### Adding new bugs

If the collections are changed in a way that makes a test fail, but the change
is important, one may wish to simply add a new bug.  After entering the bug in
the issue tracker, find the failing collection(s) in `replacements.tests`.  You
will find something like

```
Int collection.SortedSet[Int]
flags --> S SORTED
$NEW --> (collection.SortedSet.empty[Int] ++ $ )
```

Just add the issue number to the `flags` line, or create the line if it doesn't
exist.  Note--there must be no blank lines between the name of the collection
and the various settings applied to it!

Good:

```
Int collection.SortedSet[Int]
flags --> S SORTED SI1234
$NEW --> (collection.SortedSet.empty[Int] ++ $ )
```

Bad:

```
Int collection.SortedSet[Int]

flags --> S SORTED SI1234
$NEW --> (collection.SortedSet.empty[Int] ++ $ )
```

Then, in `single-line.tests` find the offending line(s) and mark them to reject
that flag.

Before:

```
S !M ... x theSameAs x.`keySet`
```

After:

```
S !M !SI1234 ... x theSameAs x.`keySet`
```

Run the tests again, and the `keySet` law will no longer be run on `SortedSet`.


#### Testing existing bugs

After a bug has been fixed, it could (potentially) be removed from collections
tests.  However, ideally, scala-collections-tests should succeed without error
on the latest released version of Scala.  To allow this, do the following.

1. Use a local copy of Scala as described in Catching a Regression.

2. Use a local copy of scala-reflect.jar as described in Catching a Regression.
Make sure you do not commit `laws/build.sbt`.

3. In the file `laws/src/main/resources/deflag-version.map`, make sure there is
a line corresponding to the latest Scala release and to the next release:

```
2.11.4 -->
2.11.x -->
```

4. Change `scalaVersion in ThisBuild` in the (root) `build.sbt` to the next
release version.

5. Add any flags you want to try removing in `deflag-version.map`:

```
2.11.x --> SI1234
```

(This change is reasonable to commit, since presumably everyone using 2.11.x will
find the bug fixed.)

If the bug is in fact fixed, the convention is to mark it with the name of the last
release where it is still present:  `SI1234` becomes `SI1234IN11X4`.  For now you
must use only caps or numbers (and start with a letter).

#### Removing existing bugs

If you find that a bug has been fixed in the latest released version of Scala,
you may wish to remove the flag.  Simply find all instances in `single-line.tests`,
`replacements.tests`, and `deflag-version.map` and delete them.

If this would make a line test have nothing before the `...`, delete `...` as well.
If it leaves `flags -->` empty in a `replacements.tests` collection, delete the
`flags` line.  You may also wish to delete previously released versions from the
list in `deflag-version.map`.


### Adding or altering laws

Because generated code contains the line numbers of the laws in `single-line.tests`,
any change that alters the line numbers will result in a lot of recompilation.  If
you're trying to go through test cycles quickly, try to get the file into the right
shape first, then work on content.

First, read the comments at the top of `single-line.tests`.  These contain brief
instructions on how the file is formatted and what the convention for various
variables means.  In brief, a law is a line of the form

```
requirements ... code with `methods` to test marked in `backticks`
```

If there are no requirements, the `...` should be omitted.  Any collection that
contains the methods marked in backticks will have that test run, unless it fails
to meet the other requirements.

The requirements are a mix of variables (lower case) that are to be provided by
text replacement, and flags that must either be present on the collection to test
(`FLAG`) or must be absent (`!FLAG`).

For example, this line:

```
y !S ... x.`++`(y).`size` == x.size + y.size
```

has a requirements section that says a variable `y` must be provided (which, by
convention and as specified in `replacements.tests`, is a collection of the same
type as being tested), and that the collection must not contain flag `S`, which
is used on set-like collections.

The code section specifies that the `size` and `++` methods must be present, and
defines a law that says non-set-like collections when joined must have exactly
as many elements as in the two original collections.

Note that whitespace in `single-line.tests` is irrelevant to the generated code
save for which line numbers are produced.  Comments are stripped off before code
is generated.

#### Altering an existing law

Altering an existing law is typically easier than writing a new one.  Let's suppose
we see this line:

```
M ... x.`keys`.toSet theSameAs x.`keySet`
```

and decide that we should use equality rather than `theSameAs` to compare the
two different ways of coming up with a set of keys for a map.  We try
the following:

```
M ... x.`keys`.toSet = x.`keySet`
```

And generate a whole mess of compilation errors that look like

```
[error] /wherever/scala-collections-laws/tests/target/scala-2.11/src_managed/main/Test_collection_Map_Long_String_.scala:1022: value toSet_= is not a member of Iterable[Long]
[error]       if (!{x.keys.toSet = x.keySet}) { throw new AssertionError("Test line 364 with "+message) }
[error]                    ^
```

because, of course, we meant `==` instead of `=`.  (As a general rule, it's a good
idea to try out the test in the REPL first--define any variables you might need,
then paste the line in.)

Note that the error messages refer to the lines in generated code, which you can
inspect (in `tests/target/scala-2.11/src_managed/main`) if you need to see the
context.

In any case, once we fix the law to be

```
M ... x.`keys`.toSet == x.`keySet`
```

the tests pass.

#### Adding a new law

To add a new law, you need to decide on the requirements--which sorts of input
are required? are any flags needed?--in addition to writing the law and
decorating the appropriate method names with backquotes.

Let's suppose we want to add a new law to test that foldLeft really operates
from left to right.  If this is true, then the last element inspected should
be the last element in the collection.  We thus conceive of the law like so:

```
x.foldLeft(?)((a,b) => b) == x.last
```

What goes in place of `?`?  We decide that any old element of the type that belongs
in the collection is okay--we don't really need to test variants--so we pick
`zero`.  It also seems like this isn't appropriate for sets and maps, so we use
the `!S` flag.  Our first try at a test looks like this:

```
zero !S ... x.`foldLeft`(zero)((a,b) => b) == x.last
```

which we put at the end of `single-line.tests`.  (In general it's good to group
tests by topic when possible.)

We try compiling this but

```
[error] /wherever/scala-collections-laws/tests/target/scala-2.11/src_managed/main/Test_collection_Iterator_Int_.scala:1943: value last is not a member of Iterator[Int]
[error]       if (!{x.foldLeft(zero)((a,b) => b) == x.last}) { throw new AssertionError("Test line 422 with "+message) }
[error]                                               ^
```

Oops!  We need to annotate _all_ the methods that are required, and we forgot `last`.  We backquote `last`, and get

```
67 collections crashed during tests with an unanticipated exception:
  collection_immutable_IndexedSeq_Int_
    (lots more here)
  collection_immutable_List__String_Long__
Details follow.
####################
# /=========
# | empty.last
# |   scala.collection.immutable.Vector.last(Vector.scala:197)
# |   tests.generated.collection.Test_collection_immutable_IndexedSeq_Int_$.test_x_zero(Test_collection_immutable_IndexedSeq_Int_.scala:2677)
# |      (lots more here)
# \=========
#
# (lots more like the above)
####################
```

because empty collections are tested also.  So we try

```
zero !S ... x.`foldLeft`(zero)((a,b) => b) == x.`lastOption`.getOrElse(zero)
```

and the tests pass.

(This law is not in the collections tests because it's not really comprehensive
enough; it's better to check that all elements are visited in order.)


#### Removing a law

Delete it.  When tests are run, it will report one fewer test.  That's it!


### Adding or altering collections

Collections are specified in `replacements.tests` in `laws/src/main/resources`.
The file essentially consists of mappings that drive text-based replacements that
generate the test files (hence the name).

Each collection must be separated from the others by at least two lines of complete
whitespace (no comments).  Read the comments at the top of the file for an outline
of the different features and customs of the replacements.

For our purposes it is sufficient to know that there is a _generic mapping_ that
applies to all collections that contain a particular type, e.g.

```
// The generic data supplied for collections that contain Ints
Int *
X --> $NEW((0 to 3)) $NEW((0 until 0)) $NEW((0 to 20 by 3)) $NEW((0 to 64)) $NEW(List[Int](0,1,2,0,1,1,1,2)) $NEW((0 to 0))
F --> ((_i: Int) => _i + 1)
  (more lines here)
```

This says that these mappings are the generic ones for elements of type `Int` (`*`
means any collection).  The line starting with `X` defines the different collection contents
tested--six variants produced from ranges via the `$NEW` macro (to be defined later).
The next line, starting with `F`, will produce an `Int => Int` function when expanded.

Further down, these expansions are mapped into variables by lower case mappings:

```
f --> $PRE(val f = @F)
```

which says to provide variable `f` by generating a val in the preamble of the test
(i.e. before the loops over different conditions).

All the specific collections appear below the generic definition.  Each individual
collection variant looks something like

```
Int collection.immutable.TreeSet[Int]
flags --> S SORTED SI6462
LET --> val
$NEW --> collection.immutable.TreeSet[Int]( $ : _* )
```

The first line says the collection holds `Int`s and that the type of the collection
is `collection.immutable.TreeSet[Int]`.  This also specifies the filename
(all non-alphanumeric characters will change into underscores) unless the `NAME`
mapping is given.  `flags` select which laws to run.  `LET` is either `val` or `def`,
depending on whether the collection might be side-effecting; `immutable.TreeSet` is
immutable, so it's safe to store it in a val.  (Otherwise it will be regenerated
each time it is used.)  Finally the `$NEW` line specifies how to generate a
`TreeSet` from the `X` variants given in the generic mappings.  Here, the companion
object apply method will do the trick (with varargs forwarding).

#### Creating a new collection

Let us suppose we want to try adding `Option` to the collections tests since it
shares a number of methods with collections.  Somewhere below the `Int *`
generic section, and above the next generic section, we add the following:

```
Int Option[Int]
LET --> val
$NEW --> ( $ .headOption )
```

Unfortunately, too many tests really require a collection, and we get many
compilation errors looking like:

```
[error] /wherever/scala-collections-laws/tests/target/scala-2.11/src_managed/main/Test_Option_Int_.scala:359: Cannot prove that List[Int] <:< Option[B].
[error]         if (!{x.flatMap(xi => y.toList.take(( xi ))) theSameAs x.map(xi => y.toList.take(( xi ))).flatten}) { throw new AssertionError("Test line 90 with "+message) }
[error]                                                                                                   ^
```

At this point, the sensible strategy is to not try, rather than retrofit all the offending tests.

We delete the lines **and run `sbt clean` to remove the generated source files**.  Removing
a test without running clean will leave the stale source file lying around to cause problems.
(You could also delete the file manually.)

Instead we try to test a map with a given default value.  In the
`(Long,String) *` section we add

```
(Long,String) collection.immutable.Map[Long,String]
flags --> S M
LET --> val
$NEW --> collection.immutable.Map[Long,String]( $ ).withDefaultValue("")
```

But this doesn't work:

```
[error] /wherever/scala-collections-laws/inst/target/scala-2.11/src_managed/main/Instances.scala:94: inst_collection_immutable_Map_Long_String_ is already defined as value inst_collection_immutable_Map_Long_String_
[error]   val inst_collection_immutable_Map_Long_String_ = (classOf[collection.immutable.Map[Long,String]], collection.immutable.Map[Long,String]( 0L->"" ).withDefaultValue(""))
```

because we already have a `collection.immutable.Map[Long,String]` without a default value.

To solve this, we set the `NAME` mapping:

```
NAME --> collection.immutable.Map[Long,String]WithDefaultValue
```

Note that spaces aren't allowed.  Now that it has a distinct name it gets distinct
variables and file names and the compilation proceeds.  However, a test fails:

```
1 collections failed assertions:
  collection_immutable_Map_Long_String_WithDefaultValue on lines 380
Details follow.
/=========
| Test line 380 with x = Map(0 -> wishes, 1 -> fishes, 2 -> dishes); zero = (0,) in group 1
|   tests.generated.collection.Test_collection_immutable_Map_Long_String_WithDefaultValue$.test_x_zero(Test_collection_immutable_Map_Long_String_WithDefaultValue.scala:1365)
|   tests.generated.collection.Test_collection_immutable_Map_Long_String_WithDefaultValue$$anonfun$25.apply$mcV$sp(Test_collection_immutable_Map_Long_String_WithDefaultValue.scala:1415)
| ...
\=========
```

Line 380 is

```
zero M ... tryO(x.`default`(zero._1)).isEmpty
```

which, in retrospect, is exactly what we expect: our new collection does not
throw an exception (and thus give `None` in `tryO`) for its default.

If we wanted to make this test succeed, we could write a second test to check
defaults that are there and add a flag to pick the correct test.  More general,
though, would be to add a mapping in the generic `(Long,String) *` section

```
DVAL --> None
```

and override it in our new collection variant with

```
DVAL --> Some("")
```

and then change the test to

```
zero M ... tryO(x.`default`(zero._1)) == @DVAL
```

The default-value collection is of limited use, however, since there is a specific
`withDefaultValue` law, and the implementations of default values tend to be very
straightforward.

So it is best to leave out this collection and leave the tests as they are.

#### Altering collections in a way that causes many tests to fail

Sometimes one finds a whole new class of bugs that one wants to test--corner
cases in inputs, for example--but it causes many test/collection pairs to fail.
These tests ideally would only be run in versions after the released version.
So we would like to gate the change by flags, but flags only disable or enable
a test at a time.

To solve this problem there are _flag-gated macros_.  Rules for these are placed
in the appropriate position in `replacements.tests`:

```
MACRONAME? --> FlagTrueOption FlagFalseOption
```

which can be used like so in `single-line.laws` (or in other macros in `replacements.tests`).

```
foo(x) == @MACRONAME?FLAGNAME
```

For example, if we wanted to change the particular values for `CR` (which is the source
of values for the `r` variable) in the generic `Int *` section, but this caused crashes
before 2.11.4, we might do the following.

First, we modify

```
CR --> List[Int](0, 1, 2, 5, 15, 28, 33, 104)
```

to

```
CR --> @CRPICK?OLDCR
CRPICK? --> @CROLD @CRNEW
CROLD --> List[Int](0, 1, 2, 5, 15, 28, 33, 104)
CRNEW --> List[Int](0, 1, 2, 5, 15, 28, 33, 104, 999)
```

(We could also put the two options on the `CRPICK?` line, but it's easier to go
back to a single version if we do it this way.)

We also add the default flag

```
flags --> OLDCR
```

to the `Int *` section, which will add that flag to every collection, and
finally append `OLDCR` to every version of Scala that should pass the tests
in `deflag-versions.map`.

**Note: this is awkward, so it will change**


#### Creating a new contained type (generic + specific)

This is a little trickier.  Use the existing patterns as a guide, and be aware
that each collection you want included must be copied each time.  (If it becomes
necessary to test many different data types, this restriction could be lifted.)

Mostly--good luck!  You're on your own!
