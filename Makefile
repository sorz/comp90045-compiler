GHC=ghc

Paz: Paz.hs PazLexer.hs PazParser.hs PazFormat.hs
	$(GHC) Paz.hs

.PHONY: clean
clean:
	rm *.o *.hi Paz
