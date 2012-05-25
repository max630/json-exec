
%.hi: %.hs
	ghc $(GHC_OPTS) --make $^
