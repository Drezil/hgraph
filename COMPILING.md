# Compiling hgraph

1.  initialize the sandbox

		cabal sandbox init

2.  install dependencies

		cabal install --only-dependencies 

3.  build package

		cabal build

4.  Optional build documentation (Requires haddock - installable with cabal install haddock)

		cabal haddock

5.  Run with

		./hgraph sampledata.adj sampledata.adj.atr sampledata.p +RTS -N<Number of Cores>

	and redirect the output into desired target.
