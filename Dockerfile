FROM haskell:7.8

WORKDIR /source

ADD . /source

#RUN apt-get update -q && apt-get install -qy \
#    texlive-full \
#    python-pygments gnuplot

RUN cabal update && cabal install --global \
	QuickCheck \
	logict-0.6.0.2 \
	parsec
	
RUN runhaskell Setup.lhs configure --user

RUN runhaskell Setup.lhs build

CMD ["bash", "run.sh"]

CMD ["cp", "/source/build/robotone.tex", "."]
