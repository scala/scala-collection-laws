C = scalac -J-Xmx2G
S = scala -J-Xmx2G
G = mkdir -p generated-tests
TESTS = replacements.tests single-line.tests
RUN = --run=3 --scalac=fsc ${TESTS}
PRERUN = fsc -shutdown; sleep 2; ${G}

default : laws/Laws.class

laws/Laws.class : laws/Parsing.class laws/MethodFinder.class Laws.scala
	${C} Laws.scala

laws/Parsing.class : Parsing.scala
	${C} Parsing.scala

laws/MethodFinder.class : MethodFinder.scala
	${C} MethodFinder.scala

gen : default
	${G}; ${S} laws.Laws ${TESTS}

run : default
	${PRERUN}; ${S} laws.Laws ${RUN}

full : default
	${PRERUN}; ${S} laws.Laws --recompile ${RUN}

docs : default
	mkdir -p api; scaladoc -d api -J-Xmx2G Parsing.scala MethodFinder.scala Laws.scala

clean :
	rm -r laws/*.class generated-tests/*.scala tests api
