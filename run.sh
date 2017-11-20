set -e
cd laws
sbt package run
cd ../tests
sbt -J-Xmx6G -J-XX:MaxMetaspaceSize=4G test
echo 'All laws succeeded to the expected extent.'
