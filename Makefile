


%.hi: %.hs
	ghc $(GHC_OPTS) --make $^

%: %.hs
	ghc $(GHC_OPTS) -main-is $@.main --make $^
