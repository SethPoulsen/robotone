#robotone is currently build-able under GHC 7.8.3. Updating GHC will break the build.
FROM haskell:7.8
MAINTAINER Khoa Nguyen <knguyen18@wooster.edu>

# Requires Haskell packages
RUN cabal update && cabal install --global \
	QuickCheck \
	logict-0.6.0.2 \
	parsec
