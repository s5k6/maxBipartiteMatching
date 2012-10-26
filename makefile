delegate = matcher testMatcher
phony = all clean help

.PHONY : $(phony) $(delegate)

help :
	 # phony    : $(phony)
	 # producing: $(delegate)
	 # You may want to use `cabal install --prefix=${HOME}/opt --user`

all :  $(delegate)
	 # Made all.  See `make help` for a list of useful targets.

clean :
	rm -rf bin $(delegate)

testMatcher :
	ghc --make -outputdir bin -o testMatcher -main-is TestMatcher TestMatcher.lhs

matcher :
	ghc --make -outputdir bin -o matcher -dynamic -main-is Matcher Matcher.lhs
	strip matcher
