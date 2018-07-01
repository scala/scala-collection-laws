set -e
sbt laws/package laws/run
sbt -J-Xmx6G -J-XX:MaxMetaspaceSize=4G -J-XX:-OmitStackTraceInFastThrow tests/test
echo 'All laws succeeded to the expected extent.'
