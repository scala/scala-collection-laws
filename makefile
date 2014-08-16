default : laws/Laws.class

laws/Laws.class : laws/Parsing.class laws/MethodFinder.class Laws.scala
	scalac -J-Xmx2G Laws.scala

laws/Parsing.class : Parsing.scala
	scalac -J-Xmx2G Parsing.scala

laws/MethodFinder.class : MethodFinder.scala
	scalac -J-Xmx2G MethodFinder.scala

gen : default
	scala -J-Xmx2G laws.Laws replacements.tests single-line.tests

run : default
	scala -J-Xmx2G laws.Laws --run=3 replacements.tests single-line.tests

fullrun : default
	scala -J-Xmx2G laws.Laws --run=3 --recompile replacements.tests single-line.tests

docs : default
	mkdir -p api; scaladoc -d api -J-Xmx2G Parsing.scala MethodFinder.scala Laws.scala

clean :
	rm -r laws/*.class generated-tests/*.scala tests api
