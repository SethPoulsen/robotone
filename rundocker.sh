docker build -t test .
docker run test

id=$(docker create test)
docker cp $id:/source/build/robotone.tex - > ./build/tmp.tex
docker rm -v $id


# delete random chars on top of copied .tex file
str='\\documentclass[a4paper,twoside,12pt]{article}'
sed "1s/.*/$str/" ./build/tmp.tex > ./build/robotone2.tex

# compile .tex file and remove tmp file
cd ./build
#xelatex robotone2.tex -quiet
xelatex "\def\showsteps{1} \input{robotone2.tex}"
rm tmp.tex
cd ..
