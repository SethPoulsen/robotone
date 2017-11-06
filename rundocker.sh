#!/bin/bash

docker=$(which docker)
xelatex=$(which xelatex)

#docker build -t robotone . #uncomment this line out if building new Docker image

# enter desired output file
outfile=$1 #path to desired output file
showsteps=$2

if [[ -n "$outfile" ]]; then # If command-line argument present
	outfile=$(basename "$1")
else
	echo "Usage: ./rundocker.sh <path_to_output_file> (--flags)"
	echo "Optional flag:"
	echo "--show-steps : Print full proofs with all steps in .tex file"
	exit
fi

# # create container for the robotone image
# docker create -v $(pwd):/root-robotone --name cont_robotone robotone /bin/bash
# wait

# # start that container
# docker container start cont_robotone
# wait

# write the proofs 
"$docker" "exec -i cont_robotone bash -c" "cd root-robotone && bash run.sh /root-robotone/build/$outfile"
wait

# # stop that container
# docker container stop cont_robotone
# wait

# docker container rm cont_robotone
# wait

# copy .tex file from container to host
pushd "$(pwd)/build"
cp $outfile $1
wait
rm $outfile
wait
popd

pushd "$(dirname $1)"
# latex compile to generate pdf
#read -p "Show proof steps? Y/N " yn
if [[ -n "$showsteps" && $showsteps == "--show-steps" ]]; then 
    "$xelatex" "\def\showsteps{1} \input{$outfile}" 
else	
	"$xelatex" "\input{$outfile}" 
fi
popd
