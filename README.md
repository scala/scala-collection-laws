scala-collections-laws
======================

Partially automatic generation of tests for the Scala collections library.  The
goal is to have a quasi-comprehensive set of collections tests that will catch
regressions in basic functionality in any collection.

These tests are expressed as a series of "laws": one-liners (mostly) that express
relationships that should be true of some or all collections.

These laws are written into a series of collection-specific tests by a code generator
(so that implicit resolution and the like is also tested) which can be run with
a method call so the results can be inspected, or can be run from the command-line
with results printed out, or can be invoked by JUnit as a standard test.

## Warning--this is a complete rewrite from earlier versions!

Earlier versions of scala-collections-laws had several weird text-based DSLs.  This
has been abandoned in favor of plain Scala code with the `sourcecode` plugin helping
to produce meaningful automatic reports.

## Quick start

Clone the repository and run

```bash
bash run.sh
```

on a Unix-based machine.  It should produce output that includes stuff like

```
[info] Running laws.GenerateAll
Generated code for 88 collection/element combinations
  88 updated since last run
```

This is the code generator and law framework in the `laws` folder generating
source files in `tests`.  The tests are then compiled, and the output
continues with

```
[info] Compiling 87 Scala sources to /home/kerrr/code/scala/lightbend/laws/tests/target/scala-2.12/classes...
[info] Compiling 1 Scala source to /home/kerrr/code/scala/lightbend/laws/tests/target/scala-2.12/test-classes...
Untested methods in ImmInt_BitSet:
  ^                  andThen            apply              compare            
  compose            empty              firstKey           from               
  ...
```

and finally ends with

```
Untested methods in Root_Iterator:
  hasDefiniteSize    isTraversableAgain sliding$default$2  
3 laws never used
  #980   val x0 = x; x0.`reduceToSize`(n); x0.`size` == n
  #1018  { val x0 = x; x0.`trimEnd`(n); x0 sameAs x.`dropRight`(n) }
  #1020  { val x0 = x; x0.`trimStart`(n); x0 sameAs x.`drop`(n) }
[info] Passed: Total 86, Failed 0, Errors 0, Passed 86
```

Congratuations!  Your collections were quasi-comprehensively tested.

## Common tasks

### Catching a regression

Catching a regression typically does not require deep knowledge of the workings
of scala-collections-laws.  Simply change the target version of Scala in the
`tests/build.sbt` file (and in `laws/build.sbt` if the change is not binary
compatible with the version already in `laws/build.sbt`) and use `run.sh` again.

#### Regression class 1: expected relationship fails.

The test files contain methods that try different combinations of inputs to laws
that require the same inputs.  If a single assertion fails in one of these
methods, the testing in that method terminates.

For example, suppose we expected `Iterator` to obey

```scala
x.`hasNext` == x.`drop`(1).hasNext
```

(the backquotes indicate that the collection must have that method for the test to run).

In addition to the normal report, at the end we would see something like

```scala
1 laws never succeeded on line
  #1054 failed 2 times      x.`hasNext` == x.`drop`(1).hasNext
*****************************************
********** Failures in line 1054
*****************************************
****** Test_Root_Iterator_Int ******
Fail(x.`hasNext` == x.`drop`(1).hasNext
// @ Laws.scala, line 1054
,Failed(Test_Root_Iterator_Int @ Test_Root_Iterator_Int.scala, line 6
  Numbers: 0, 0, -1, 0, 0
  Provider: singleton 0 with
    Iterator(0) ; len 1
    Iterator() ; len 0
  Ops
    plusOne   @ Ops.scala, line 136
    bit33     @ Ops.scala, line 146
    summation @ Ops.scala, line 156
    mod3      @ Ops.scala, line 166
    halfEven  @ Ops.scala, line 178
),Some(Test_Root_Iterator_Int @ Test_Root_Iterator_Int.scala, line 6
  Numbers: 0, 0, -1, 0, 0
  Provider: singleton 0 with
    Iterator(0) ; len 1
    Iterator() ; len 0
  Ops
    plusOne   @ Ops.scala, line 136
    bit33     @ Ops.scala, line 146
    summation @ Ops.scala, line 156
    mod3      @ Ops.scala, line 166
    halfEven  @ Ops.scala, line 178
),None)

****** Test_Root_Iterator_Str ******
...
(very similar information repeats regarding `String` element type)
...
************************************
************************************
************** 2 errors
************************************
************************************
[error] Test laws.Test_Everything_With_JUnit failed: assertion failed
[error] Failed: Total 87, Failed 1, Errors 0, Passed 86
[error] Failed tests:
[error] 	laws.Test_Everything_With_JUnit
[error] (test:test) sbt.TestsFailedException: Tests unsuccessful
```

Every time the law fails--for every collection that runs it, for every element
type that is used for it--it will appear in the output list.  Consequently,
the list can be very long if something major has gone wrong.

In this case, there is only a single failure and the information provided allows
us to replicate it in the REPL easily.  In this case, we can see by inspection
that the law is silly and go to `Laws.scala` line 1054 to fix it.

In the case of errors that are more mysterious, the test prints out enough information
to manually reconstruct the test case, albeit with a little effort.  In this case, one
can manually create the `Numbers`, `Ops`, and `Instance` values like so:

```
tests$ sbt -J-Xmx6G -J-XX:MaxMetaspaceSize=4G
[info] Loading global plugins from /home/kerrr/.sbt/0.13/plugins
[info] Set current project to collections-laws-tests (in build file:/home/kerrr/code/scala/lightbend/laws/tests/)
> console
[info] Starting scala interpreter...
[info]
Welcome to Scala 2.12.4 (OpenJDK 64-Bit Server VM, Java 1.8.0_151).
Type in expressions for evaluation. Or try :help.

scala> import laws._
import laws._

scala> val num = Numbers(0, 0, -1, 0, 0)
num: laws.Numbers = Numbers: 0, 0, -1, 0, 0

scala> val ops = Int
Int            IntTest   Integer2int   InternalError          
IntGenerator   Integer   Integral      InterruptedException   

scala> val ops = Str
StrGenerator       StrictMath           StringContext                     
StrLongGenerator   String               StringFormat                      
StrLongTest        StringBuffer         StringIndexOutOfBoundsException   
StrTest            StringBuilder                                          
Stream             StringCanBuildFrom                                     

scala> val ops = Ops(Ops.IntFns.plusOne, Ops.IntToLongs.bit33, Ops.IntOpFns.summation, Ops.IntPreds.mod3, Ops.IntParts.halfEven)
ops: laws.Ops[Int,Long] =
Ops
  plusOne   @ Ops.scala, line 136
  bit33     @ Ops.scala, line 146
  summation @ Ops.scala, line 156
  mod3      @ Ops.scala, line 166
  halfEven  @ Ops.scala, line 178

scala> val inst = InstantiatorsOfInt.Root.iterator().apply(0, Array(0), Array.empty[Int])
inst: laws.Instance[Int,Iterator[Int]] =
Provider: singleton 0 with
  Iterator(0) ; len 1
  Iterator() ; len 0
```

and used in a custom source file in the `tests` directory; or the variables used
can be manually filled in and the test pasted in inline (not very interesting in
this case; it's just `def x = Iterator(0)`--make sure to use `def` for collections
with state!).

In principle one ought to be able to create a new test instance with e.g.
`val test = new Test_Root_Iterator_Int(num, ops, inst, 1054)` inside SBT, but
I've hit classloader issues before, so don't count on this working.

#### Regression class 2: runtime exception

The test framework tries to catch runtime exceptions.  Generally, as long as the
collection can be built without error, the information will be similar to the
relationship failure class.  For instance, if we decide arrays should obey

```scala
"xsize > 0 implies x(xsize-1) == x(-1)".law(ARR)
```

(which it would if we had negative indices running from the end of the array), we
get the following:

```
********** Failures in line 1054
*****************************************
****** Test_Mut_Array_Str ******
Fail(xsize > 0 implies x(xsize-1) == x(-1)
// # ARRAY
// @ Laws.scala, line 1054
,Threw(Test_Mut_Array_Str @ Test_Mut_Array_Str.scala, line 6
  Numbers: 0, 0, -1, 0, 0
  Provider: singleton  with
    Array(0) ; len 1
    Array() ; len 0
  Ops
    upper      @ Ops.scala, line 141
    natural    @ Ops.scala, line 151
    concat     @ Ops.scala, line 161
    increasing @ Ops.scala, line 172
    oddMirror  @ Ops.scala, line 184
,java.lang.ArrayIndexOutOfBoundsException: -1),Some(Test_Mut_Array_Str @ Test_Mut_Array_Str.scala, line 6
  Numbers: 0, 0, -1, 0, 0
  Provider: singleton  with
    Array(0) ; len 1
    Array() ; len 0
  Ops
    upper      @ Ops.scala, line 141
    natural    @ Ops.scala, line 151
    concat     @ Ops.scala, line 161
    increasing @ Ops.scala, line 172
    oddMirror  @ Ops.scala, line 184
),Some(java.lang.ArrayIndexOutOfBoundsException: -1))
```

The test, array and element type, and exception thrown are all visible from inspection,
and replicating the error can be done as in the previous case.

If, however, there is an exception during creation of the collection, the error
reporting is less complete.  For instance, if we change iterator creation from

```scala
val iterator = C(a => (new IteratorKnowsSize[A](a)): Iterator[A])
```

to

```scala
val iterator = C(a => {
    if (a.length < 10) new IteratorKnowsSize[A](a)
    else (for (i <- 0 until a.length*2) yield a(i)).iterator
  })
```

we get the following error:

```
********** Failures in line 184
*****************************************
****** Test_Root_Iterator_Int ******
Fail(x sameType x.`map`(f)
// # !BITSET_MAP_BREAKS_BOUNDS !SUPER_ITREES !SUPER_MOPENHM
// @ Laws.scala, line 184
,Error(java.lang.ArrayIndexOutOfBoundsException),None,Some(java.lang.ArrayIndexOutOfBoundsException))
```

plus similar errors for every other test that `Iterator` has.  This situation
could be improved, but presently, this is all you have to go on.

#### Regression class 3: compilation error

The presumption is that compilation errors will be rare.  If a change has caused
a compilation error, you will have to work through SBT's error reporting facilities
and look at the generated code to figure out what went wrong.  Since each method
is named after the line that the law came from, you can generally quickly get back
to the offending law.

In some cases, the code generation itself may be inappropriate.  In this case,
the code generation routines in `Generator#code` and/or `GenerateAll` (both in
Generator.scala) should be examined.


### Dealing with collections bugs

The collections tests will not pass successfully if even a single error is found.
This requires the entire test-suite to avoid any existing bugs in the source code.

The convention for bugs is to create a new flag (in Flag.scala) with the issue
number, e.g.

```scala
val ISSUE_1234 = T
```

Then, you decorate each affected collection with the bug, e.g. by changing
the collection instantiator in `InstantiatorsOf[A]#Imm` from

```scala
val seq         = C(_.to[collection.immutable.Seq], SEQ)
```

to

```scala
val seq         = C(_.to[collection.immutable.Seq], SEQ, ISSUE_1234)
```

Finally, you can create positive and negative versions of each test, as necessary.

For instance, if the functionality is simply broken, you would write a law like

```scala
"x.`foo` sameAs x".law(ISSUE_1234.!)
```

On the other hand, if you want to verify the undesired behavior, you can write an
alternate test for the buggy case:

```scala
"x.`foo` sameAs x".law(ISSUE_1234.!)

"x.`foo` sameAs x.`reverse`".law(ISSUE_1234)
```

When the bug is fixed, it should be removed from the test sources.  Presently
there is no ability to have a single set of laws that handles multiple versions
of Scala with different sets of bugs, but git branches can be used to maintain
slightly different versions of the source for each Scala version.

# TODO -- EDIT BELOW HERE

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
