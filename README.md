scala-collections-laws
======================

An exploration of partially automatic generation of tests for the entire collections library.

Compile Parsing.scala and MethodFinder.scala, then Laws.scala.

Run with scala laws.Laws replacements.tests single-line.tests

If you want to run the tests, add --run (--run=4 will try to run 4 at a time)

If you want to only run tests that have been modified and therefore probably need to be recompiled, use --changed

If you want to recompile everything whether or not the source was changed on this run, use --recompile

Laws.scala will create and compile Instances.scala, and will create a whole bunch of source in generated-tests and class files in tests.generated.collection (be warned).
