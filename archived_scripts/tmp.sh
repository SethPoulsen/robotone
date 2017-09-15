#!/usr/bin/bash
wd=$(dirname "$0")
xelatex=$(which xelatex)

"$wd/dist/build/robotone/robotone" > "$wd/build/robotone.tex"
echo "TeX"
pushd "$wd/build"
"$xelatex" "\input{robotone.tex}" -jobname=robotoneshort -quiet
"$xelatex" "\def\showsteps{1} \input{robotone.tex}" -jobname=robotone -quiet
popd

# LaTeX compile
echo "TeX file created."
#pushd "$wd/build"

read -p "Show steps? (Y/N) " yn
    case $yn in
        [Yy]* ) "$xelatex" "\def\showsteps{1} \input{$1}" -jobname=robotone -quiet; break;;
        * ) "$xelatex" "\input{$1}" -jobname=robotoneshort -quiet;;
    esac
popd

# id=$(docker create test)
# docker cp $id:/source/build/robotone.tex - > ./build/tmp.tex
# docker rm -v $id


# # delete random chars on top of copied .tex file
# str='\\documentclass[a4paper,twoside,12pt]{article}'
# sed "1s/.*/$str/" ./build/tmp.tex > ./build/robotone2.tex

# # compile .tex file and remove tmp file
# cd ./build
# #xelatex robotone2.tex -quiet
# xelatex "\def\showsteps{1} \input{robotone2.tex}"
# rm tmp.tex
# cd ..
