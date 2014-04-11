How to build:
$ cabal install --only-dependencies
$ cabal configure
$ cabal build

Create documentation:
$ cabal haddock --executables

Build and create documentation with all hyperlinks
$ cabal install --only-dependencies --enable-documentation
$ cabal configure
$ cabal build
$ cabal haddock --executables

Run:
$ dist/build/hgraph/hgraph --help

How to build (sandboxed, >=cabal 1.18, recommended): 
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal configure
$ cabal build

How to build (sandboxed, cabal-dev, deprecated): 
$ cabal-dev install --only-dependencies
$ cabal-dev configure
$ cabal-dev build

