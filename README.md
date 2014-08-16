scala-collections-laws
======================

An exploration of partially automatic generation of tests for the entire collections library.

To generate the tests, `make`

To generate and run the tests, `make run`

To generate, recompile even tests whose source hasn't changed, and run, `make fullrun`

By default, it uses three threads at a time to compile/run (--run=3).  This can be altered in the makefile.

If you want to only run tests that have been modified and therefore probably need to be recompiled, use --changed (this is not in the makefile)

If you want to recompile everything whether or not the source was changed on this run, use --recompile (`fullrun` uses this)

To specify what command to use for the compiler, use `--scalac=fsc` or somesuch.  The safest way to pass multiple arguments is to repeat the --scalac command, e.g. `--scalac=fsc --scalac=-J-Xmx2G`.  It will do its best to interpret a single argument with spaces, e.g. '--scalac=fsc -J-Xmx2G'.

To specify what command to use to run, use `--scala=scala` or somesuch.  

Laws.scala will create and compile Instances.scala, and will create a whole bunch of source in generated-tests and class files in tests.generated.collection (be warned).
