docker build -t test .
docker run test

id=$(docker create test)
docker cp $id:/source/build/robotone.pdf - > ./build/robotone2.pdf
docker rm -v $id
