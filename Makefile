
default :
	#
	# WARNING: Using `make` is outdated.  See `INSTALL` and use
	# `stack` instead.  Not all features have been ported to the
	# new build system.
	#
	false

.PHONY : dist-clean library test

# This makefile adapts to whether the file
# `tests/DanilenkoOriginal.hs` is present or not.

FLAGS =
ifneq ($(wildcard tests/DanilenkoOriginal.hs),)
  FLAGS += --flag danilenko
endif


dist/build/matcher/matcher : cabal.sandbox.config
	cabal build matcher

library : cabal.sandbox.config
	cabal install
	cabal haddock

test : cabal.sandbox.config
	cabal install --only-dependencies --enable-tests $(FLAGS)
	cabal configure --enable-tests $(FLAGS)
	cabal test

shootout : cabal.sandbox.config
	cabal install --only-dependencies $(FLAGS) --flag buildPerfTest
	cabal configure $(FLAGS) --flag buildPerfTest
	cabal build
	scripts/shootout

README.html : README.md
	pandoc -s -t html <README.md >| README.html

cabal.sandbox.config :
	cabal sandbox init

dist-clean :
	git clean -xdf
