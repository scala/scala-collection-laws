scala-collections-laws
======================

Partially automatic generation of tests for the Scala collections library.

To generate the tests, `make`

To generate and run the tests, `make run`

To generate, recompile even tests whose source hasn't changed, and run, `make full`

By default, it uses three threads at a time to compile/run (`--run=3`).  This can be altered in the makefile.
 
If you want to only run tests that have been modified and therefore probably need to be recompiled, use `--changed` (this is not in the makefile)
 
If you want to recompile everything whether or not the source was changed on this run, use `--recompile` (`full` uses this)
 
To specify what command to use for the compiler, use `--scalac=fsc` or somesuch (paths are okay; be sure to escape spaces).
 
To specify what command to use to run, use `--scala=scala` or somesuch.
 
To specify arguments, use `--scala-args=arg1` `--scala-args=arg2` etc. (one `--scalac-args=` per argument).

`Laws.scala` will create and compile `Instances.scala`, and will create a whole bunch of source in `generated-tests` and class files in `tests/generated/collection` (be warned).

### A mini-walkthrough of detecting failed tests and verifying a bug

If we execute a freshly cloned copy:

```bash
git clone https://github.com/Ichoran/scala-collections-laws
cd scala-collections-laws
make run
```

we should see something like the following (`...` indicates many more output lines, not a literal output string from the tests):

```
scalac -J-Xmx2G Parsing.scala
scalac -J-Xmx2G MethodFinder.scala
scalac -J-Xmx2G Laws.scala
scala -J-Xmx2G laws.Laws --run=3 replacements.tests single-line.tests
Compiling Instances.scala.
warning: there were two deprecation warnings; re-run with -deprecation for details
one warning found
Created 129 single-line tests for
  collection_immutable_HashSet_Int_
  18 methods not tested:
  --         addString  andThen    companion  compose    copyToArray           
  genericBuilder        isTraversableAgain    mkString   repr       scan       
  scanLeft   scanRight  subsets    tails      transpose  unzip3     updated0   
...
Created 177 single-line tests for
  collection_immutable_StreamView_Int_Stream_Int__TakenWhile
  20 methods not tested:
  andThen    applyOrElse           combinations          companion  compose    
  copyToArray           force      genericBuilder        hasDefiniteSize       
  isDefinedAt           isTraversableAgain    lastIndexOfSlice      lift       
  orElse     repr       runWith    to         transpose  unzip3     
  viewToString          

Generated 83 test files.
1 test lines were not used for any collection: 
  169 y...x.`:++`(y) theSameAs x.`++`(y)
----------
- All tests passed for collection_immutable_Iterable_Int_
----------
...
----------
- All tests passed for collection_immutable_StreamView_Int_Stream_Int__TakenWhile
----------

========= Summary of Test Run =========
Tested 83 collections in 17 minutes, 26.1 seconds
  83 collections passed
  0 collections failed
  0 collections failed to compile
```

If we look at `single-line.tests`, we find that some lines are marked with flags indicating that there may be a bug in that test for some collections.  For instance:

```
n m !BUGB ... n < 0 || m >= x.size || { sameType(x, x.`slice`(n, m)) }
```

This says that a two-argument `slice` should have the same type as the original collection.  And some collections in `replacements.texts` define this flag, so they should be skipped:

```
Int collection.immutable.Range
X  --> $NEW((0 to 3)) $NEW((0 until 0)) $NEW((0 to 20 by 3)) $NEW((0 to 64))
flags --> N RANGE BUGB
LET --> val
$NEW --> ( $ : collection.immutable.Range )
```

If we regenerate and run the tests without this flag:

```
scala -J-Xmx2G laws.Laws --deflag=BUGB --run=3 replacements.tests single-line.tests
```

we find that the summary has changed:

```
========= Summary of Test Run =========
Tested 83 collections in 1 minutes, 13.2 seconds
  82 collections passed
  1 collections failed
    tests.generated.collection.Test_collection_immutable_Range
  0 collections failed to compile
```

If we scroll up through the output, we find the summary of tests for `Range`:

```
!!!!!!!!!!
! 1 errors!
! 
! Test line 147 with m = -1; n = 0; x = Range(0, 1, 2, 3) in group 1
!   tests.generated.collection.Test_collection_immutable_Range$$anonfun$test_m_n_x$1$$anonfun$apply$mcVI$sp$17.apply$mcVI$sp(Test_collection_immutable_Range.scala:394)
!   tests.generated.collection.Test_collection_immutable_Range$$anonfun$test_m_n_x$1$$anonfun$apply$mcVI$sp$17.apply(Test_collection_immutable_Range.scala:389)
!   tests.generated.collection.Test_collection_immutable_Range$$anonfun$test_m_n_x$1$$anonfun$apply$mcVI$sp$17.apply(Test_collection_immutable_Range.scala:389)
!   scala.collection.immutable.List.foreach(List.scala:381)
!   tests.generated.collection.Test_collection_immutable_Range$$anonfun$test_m_n_x$1.apply$mcVI$sp(Test_collection_immutable_Range.scala:389)
!   tests.generated.collection.Test_collection_immutable_Range$$anonfun$test_m_n_x$1.apply(Test_collection_immutable_Range.scala:389)
!   tests.generated.collection.Test_collection_immutable_Range$$anonfun$test_m_n_x$1.apply(Test_collection_immutable_Range.scala:389)
!   scala.collection.immutable.List.foreach(List.scala:381)
!   tests.generated.collection.Test_collection_immutable_Range$.test_m_n_x(Test_collection_immutable_Range.scala:389)
!!!!!!!!!!
```

It says that the error is on test line 147; in `single-line.tests` we find that line 147 is indeed:

```
n m !BUGB ... n < 0 || m >= x.size || { sameType(x, x.`slice`(n, m)) }
```

Furthermore, it tells us what values of the parameters uncovered the error.  We fire up a REPL to test:

```scala
scala> (0 to 3).slice(0, -1)
res0: scala.collection.immutable.IndexedSeq[Int] = Vector()
```

and confirm that, indeed, this law is not obeyed by `Range`.
