


%.hi: %.hs always
	ghc $(GHC_OPTS) --make $<

%: %.hs always
	ghc $(GHC_OPTS) -main-is $@.main --make $<

always:
