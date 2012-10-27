.PHONY : default all clean distclean test

default : matcher

all : matcher testMatcher
	 # Made all.  See `make help` for a list of useful targets.

clean :
	rm -rf bin

distclean : clean
	rm -rf matcher testMatcher

test : testMatcher
	./runtest


testMatcher :
	ghc --make -outputdir bin -o testMatcher -main-is TestMatcher TestMatcher.lhs

matcher :
	ghc --make -outputdir bin -o matcher -dynamic -main-is Matcher Matcher.lhs
	strip matcher
