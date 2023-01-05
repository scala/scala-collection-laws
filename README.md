# scala-collection-laws
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

Earlier versions of scala-collection-laws had several weird text-based DSLs.  This
has been abandoned in favor of plain Scala code with the `sourcecode` plugin helping
to produce meaningful automatic reports.

## Quick start

Clone the repository, update `build.sbt` with the `scalaVersion` you want to test and
a compatible `sourcecode`, and run

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
[info] Compiling 87 Scala sources to /.../laws/tests/target/scala-2.12/classes...
[info] Compiling 1 Scala source to /.../laws/tests/target/scala-2.12/test-classes...
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

### Local testing quick start

If you want to test changes to the compiler or standard library, you will presumably want to do something like the following.

1. Fork the Scala compiler somewhere on your machine.  (You've probably done that already.)
2. Publish the build locally by running `sbt publishLocal` and note what it's called (e.g. `2.13.0-pre-SNAPSHOT`)
3. Fork [sourcecode](https://github.com/lihaoyi/sourcecode.git)
4. Alter `sourcecode`'s `build.sbt` in the following ways:
  a. In `baseSettings`, change `scalaVersion` to the locally built compiler
  b. In `baseSettings`, you may also wish to change `version` to a custom name for your local build (e.g. I would change `"0.1.5-SNAPSHOT"` to `"0.1.5-local-SNAPSHOT"`)
  c. Remove `NativePlatform` and maybe `JSPlatform` from `lazy val sourcecode = crossProject(...)`
  d. Remove the `.nativeSettings` and maybe `.jsSettings` from the end of the definition of `lazy val sourcecode`
  e. Remove `lazy val native` and maybe `lazy val js` from the end of the file
  f. Run `sbt`, enter `project sourcecodeJVM` and then `publishLocal`, noting what it's called
5. Alter `scala-collection-laws`'s `build.sbt` to request the local versions of the compiler and sourcecode

Now you can run `bash run.sh` to commence testing.  (Note--this only tests the JVM build.)

Each time you change the library or compiler, you'll need to publish both the compiler and `sourcecode` locally before running collections-laws again.

## Common tasks

### Catching a regression

Catching a regression typically does not require deep knowledge of the workings
of scala-collection-laws.  Simply change the target version of Scala in the
`build.sbt` file and use `run.sh` again.

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
),Some(...),None)

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
[info] Loading global plugins from /.../.sbt/0.13/plugins
[info] Set current project to collections-laws-tests (in build file:/.../laws/tests/)
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
, Threw(Test_Mut_Array_Str @ Test_Mut_Array_Str.scala, line 6
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
, java.lang.ArrayIndexOutOfBoundsException: -1
laws.Test_Mut_Array_Str.$anonfun$runLaw1054$1(Test_Mut_Array_Str.scala:934)
laws.Test$Logic.implies(Test.scala:239)
laws.Test_Mut_Array_Str.runLaw1054(Test_Mut_Array_Str.scala:934)
laws.Test_Mut_Array_Str.$anonfun$lawTable$196(Test_Mut_Array_Str.scala:1135)
laws.Test_Mut_Array_Str.$anonfun$runLaw$1(Test_Mut_Array_Str.scala:1138)
laws.Test_Mut_Array_Str.$anonfun$runLaw$1$adapted(Test_Mut_Array_Str.scala:1138)
scala.Option.map(Option.scala:146)
laws.Test_Mut_Array_Str.runLaw(Test_Mut_Array_Str.scala:1138)
laws.Test.run(Test.scala:123)
laws.Runner.runOne(Runner.scala:44)
laws.Runner.runNums(Runner.scala:62)
laws.Runner.runOps(Runner.scala:94)
laws.Runner.run(Runner.scala:126)
laws.Test_Mut_Array_Str$.run(Test_Mut_Array_Str.scala:1161)
...
), (), Some(...), Some(...)}

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
, Error(java.lang.ArrayIndexOutOfBoundsException: 12
scala.runtime.ScalaRunTime$.array_apply(ScalaRunTime.scala:55)
laws.InstantiatorsOf$Root$.$anonfun$iterator$2(Instantiator.scala:166)
laws.InstantiatorsOf$Root$.$anonfun$iterator$2$adapted(Instantiator.scala:166)
scala.collection.TraversableLike.$anonfun$map$1(TraversableLike.scala:234)
scala.collection.immutable.Range.foreach(Range.scala:156)
scala.collection.TraversableLike.map(TraversableLike.scala:234)
scala.collection.TraversableLike.map$(TraversableLike.scala:227)
scala.collection.AbstractTraversable.map(Traversable.scala:104)
laws.InstantiatorsOf$Root$.$anonfun$iterator$1(Instantiator.scala:166)
laws.Instance$.from(Instance.scala:95)
laws.Instance$$anon$2.apply(Instance.scala:121)
laws.Instance$$anon$2.apply(Instance.scala:120)
scala.Function3.$anonfun$tupled$1(Function3.scala:35)
scala.Option.map(Option.scala:146)
laws.Exploratory$$anon$2.lookup(Explore.scala:124)
laws.Exploratory.$anonfun$lookup$1(Explore.scala:120)
scala.Option.flatMap(Option.scala:171)
laws.Exploratory.lookup(Explore.scala:120)
laws.Exploratory.lookup$(Explore.scala:120)
laws.Exploratory$$anon$2.lookup(Explore.scala:122)
laws.Runner.run(Runner.scala:123)
laws.Test_Root_Iterator_Int$.run(Test_Root_Iterator_Int.scala:649)
...
), (), None, Some(...)}
```

plus similar errors for every other test that `Iterator` has.  This situation
could be improved (by better capturing context), but presently, this is all you
have to go on.

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

### Adding or altering laws

Laws are all specified in `Laws.scala` inside strings which are marked for
code generation by appending `.law`, possibly with arguments.

Laws should only be run on collections that actually have the appropriate methods.
In order to mark which methods in the code need to be tested, write them inside
backticks, i.e.

```
"x.`tail` == x.`drop`(1)".law
```

(Note that the above is not a valid law, as `tail` and `drop(1)` have different
behavior on empty collections.)

#### Available variables and types

Within the code of the law you have access to sixteen variables whose values will
be varied if use is detected, and three or five types:

| Type Name | Meaning |
|-----------|---------|
| `A`       | The type of element stored in the collection under test |
| `B`       | The type that `A` is mapped to via the function `g` |
| `CC`      | The type of the collection (not parametric!) |
| `K`       | Maps only: the type of the keys |
| `V`       | Maps only: the type of the values |

_Note that `CC` is the fully applied type, e.g. `Iterator[Int]`; this is
necessary in case `CC` has no type parameters or has multiple parameters,
e.g. `BitSet` or `Map[String, Long]`._

_Note that `A` is identically `(K, V)` for maps._

| Variable Name | Expected Values   | Meaning |
|---------------|-------------------|---------|
| `a`           | an element        | Some single instance of the collection's element type |
| `b`           | an element        | Some single instance of the type that `A` is mapped to via `g`
| `x`           | a collection      | May be empty or have one or more elements |
| `xsize`       | `x.size`          | Contains the pre-computed size of `x` |
| `y`           | another collection| In general is not the same as `x` (but can be) |
| `ysize`       | `y.size`          | Precomputed size of `y` |
| `n`           | 0 until `x.length`| An index into the `x` collection |
| `f`           | `A => A`          | Transformation that does not alter element type |
| `g`           | `A => B`          | Transformation that does alter element type |
| `op`          | `(A, A) => A`     | An operator that collapses elements |
| `p`           | `A => Boolean`    | A predicate that tests the elements |
| `pf`          | partial function  | A transformation defined on only some elements; does not alter element type |
| `n`           | in `0 until xsize`| An integer value that could be used as a valid index into `x` |
| `nn`          | non-negative      | An integer value that is a valid index, but maybe not for `x` |
| `m`           | in `0 until ysize`| An integer value that could be used as a valid index into `y` |
| `mm`          | non-negative      | An integer value that is a valid index, but maybe not for `y`; in general is different than `nn` |
| `r`           | integer           | An integer value that could be anything |

You can find the range of variation for these variables in `Numbers.scala` for
`n`, `nn`, `m`, `mm`, and `r`; in `Ops.scala` for `f`, `g`, `op`, `p`, and `pf`;
and in `Instantiator.scala` for `a`, `x`, and `y` (`xsize` and `ysize` are determined
by `x` and `y`).  In the last case, look for `possible_a`, `possible_x`, and `possible_y`.

#### Compilation errors

If you write a law that doesn't compile, e.g.

```
"x.`tail` = x.`head`".law
```

You will get compile errors after the code generation phase:

```
[info] Compiling 87 Scala sources to /.../laws/tests/target/scala-2.12/classes...
[error] /.../laws/tests/Test_ImmInt_BitSet_Int.scala:619: value tail_= is not a member of scala.collection.immutable.BitSet
[error]     x.tail = x.head
[error]       ^
[error] /.../laws/tests/Test_ImmKV_HashMap_Long_String.scala:571: value tail_= is not a member of scala.collection.immutable.HashMap[Long,String]
[error]     x.tail = x.head
[error]       ^
...
```

Usually this is enough to see the problem, but as the source line is also given,
one can inspect the full generated code if the error message is inadequate on its
own.

#### Restricting the collections to which the law applies

The primary way to restrict the applicability of laws is to use the flags
in `Flag.scala`.  `SEQ`, `SET`, and `MAP` are particularly useful flags, as
these collections have rather different behavior from each other.

Collections are marked with a subset of flags; in order to run only collections
that are marked, name the flag in the parameters of the `law` method:

```
"x.`reverseIterator` sameAs x.`reverse`".law(SEQ)
```

In contrast, if you want to only consider collections that do *not* have the flag,
append `.!` to the flag name:

```
"x.`+`(a).`contains`(a)".law(MAP.!)
```

Only those collections that have all the positive flags and are missing all the
negative flags will have a test generated for that law.  Note that if the conditions
are so restrictive that no collections are tested, the law will be listed as
untested in the output.

Additional testing is available at runtime.  The best way to achieve this is to
use the `Filt` helper object which allows you to query the values of the parameters
before the test is actually run.  For instance,

```
"x.`permutations`.size == x.`permutations`.toSet.size".law(Filt.xsize(_ <= 8))
```

demands that all permutations are distinct, but would take impractically long
for large collections, so it only runs when `xsize` is no more than 8.

### Adding or altering collections

#### Step one: specify how to build the collection

Collections are specified in `Instantiators.scala`.  There are objects for
each concrete element type, and within that objects for each namespace that
contain builders for the collections.  These specify how to build the relevant
collection from an array of the appropriate elements.

There is a fair bit of code duplication, as the type signatures get very hairy
if generalized; this is not clearly the right strategy, however.

In any case, if you're using an existing namespace, you can simply add the
collection to the appropriate place.  For instance, if there were a new `Rope`
collection in `collection.immutable`, one would add to `InstantiatorsOf[A]`, inside
the `Imm` object, the following line:

```scala
val rope        = C(_.to[collection.immutable.Rope], SEQ)
```

If it is a map, add to `InstantiatorsOfKV[K, V]` instead.  If the element type
must be restricted, add to the element-specific objects, e.g. `InstantiatorsOfInt`.

If the namespace is different, e.g. `collection.concurrent`, a new inner object
should be created much like `Imm`.  Cutting and pasting should be sufficient; use
`Mut` as a template if the collection has state that can alter (in which case, code
that references `x` and `y` will get freshly generated copies each time they are
named), or `Imm` if not (in which case the collection is created once and cached).

For instance, for `collection.concurrent` we would have

```scala
object Conc extends Instance.PackagePath {
  def nickname = Conc"
  def fullyQualified = "scala.collection.concurrent"
  def C[CC: TypeTag: Sizable](ccf: Array[A] => CC, flags: Flag*)(implicit nm: sourcecode.Name): Deployed[A, CC] = {
    val gen = inst.makeWith(ccf, flags: _*)(nm, implicitly[TypeTag[CC]], implicitly[Sizable[CC]])
    val ans = new Deployed[A, CC]{
      val secretly = gen
      var accesses: Int = 0
      val name = nm.value.toString
      def group = typeTagA.tpe.toString + " in " + nickname
      def apply(): Instance.FromArray[A, CC] = { accesses += 1; secretly }
    }
    registry += ans
    ans
  }

  val imaginarySkipList = C(_.to[collection.concurrent.ImaginarySkipList], SEQ)
}
```

Note that the `val` name _must_ be the collection name with the first letter lower-cased.
The generator uses this val name to generate the type signature of the class.

Finally, you must find the `val force` lines for each fully specified instantiator object
and add `:: Conc` to them (to actually add `imaginarySkipList` to the list of collections
for which to generate tests).

#### Step two: create the test code generator for the collection

Once the collection has been created, a parallel structure needs to be created in
`Generators.scala` to actually generate code for the class.  In the future, perhaps it would
be better to combine these two so one cannot specify an instantiator without a
corresponding generator.

In any case, simply add the collection to the element-type-specific generator objects,
in the appropriate place that mirrors the path to the instantiator (this is mostly just
convention, but it makes it easier to avoid mistakes).  For instance, ropes would be
added to both `AllIntGenerators` and `AllStrGenerators` inside `Imm` as

```
val rope = register(io.Imm)(_.rope())
```

while for `Conc` one would create a new `object Conc` inside `AllIntGenerators`
and `AllStrGenerators` that looked like

```
object Conc {
  val imagnarySkipList = register(io.Conc)(_.imaginarySkipList())  
}
```

and then `:: Conc` would be added to `val force` in both `All___Generators` objects.

#### Creating a new element type

Follow the examples of `BitSet` and/or `LongMap`; between them they illustrate
most or all of the issues one must address to get a specific collection type working.

You will also need to add the new element-type-generator to the `write` method
in `GenerateAll`.

## Conclusion

Thanks for reading!  Good luck!
