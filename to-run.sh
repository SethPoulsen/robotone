# Steps to run

docker container start cont_robotone

docker exec -t -i cont_robotone /bin/bash

# Now in the docker container shell do...

cabal build robotone
runghc Main.hs ../build/all.tex