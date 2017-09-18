Fork of `robotone` to be found in https://github.com/mg262/research
The work of Mohan Ganesalingham and Tim Gowers in their paper "A Fully Automatic Theorem Prover with Human-Style Output".

As the original article states: This article is distributed under the terms of the Creative Commons Attribution 4.0 International License (http://creativecommons.org/licenses/by/4.0/), which permits unrestricted use, distribution, and reproduction in any medium, provided you give appropriate credit to the original author(s) and the source, provide a link to the Creative Commons license, and indicate if changes were made.

This software has been modified from its original source to customize towards the course MATH-332 Real Analysis I at The College of Wooster, OH. 

# Build and run

Install Docker, a software container platform (details can be found at https://www.docker.com). Also make sure you have a latex engine installed.

``` bash
# in our project directory
$ git clone https://github.com/nguyen-khoa/robotone.git
$ cd robotone
```

Build the Docker image with the provided Dockerfile with: 

``` bash
$ docker built -t robotone .
```

Then create the Docker container named *cont_robotone* with: 

```bash
$ docker run -v $(pwd):/root-robotone --name cont_robotone -it robotone /bin/bash &
```

Start the container created above with:

```bash
$ docker container start cont_robotone
```
            
If you encounter errors of the form, "Error response from daemon: Container [...] is not running", use the command above.

 
Now, whenever you update the problems in `src/Problems.hs` or the theorems and definitions in `src/RealAnalysis.hs`, run the script `rundocker.sh` from this project directory with: 

```bash
$ bash rundocker.sh <path/to/desired/output/file.tex> 
```
and retrieve the generated proofs in both `.tex` and `.pdf` files where expected. 

Note, the TeX file requires the `libertine.sty` fonts, available in Debial from `texlive-fonts-extra`.
