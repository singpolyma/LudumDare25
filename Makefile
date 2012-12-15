GHCFLAGS=-Wall -XNoCPP -fno-warn-name-shadowing -XHaskell98 -O2 -threaded
HLINTFLAGS=-XHaskell98 -XNoCPP -i 'Use camelCase' -i 'Use String' -i 'Use string literal' -i 'Use list comprehension' --utf8
VERSION=0.0.0

.PHONY: all clean doc install

all: report.html doc dist/build/LudumDare25/LudumDare25 dist/LudumDare25-$(VERSION).tar.gz

install: dist/build/LudumDare25/LudumDare25
	cabal install

report.html: Main.hs SomeMap.hs Types.hs
	-hlint $(HLINTFLAGS) --report Main.hs SomeMap.hs Types.hs

doc: dist/doc/html/LudumDare25/index.html README

README: LudumDare25.cabal
	tail -n+$$(( `grep -n ^description: $^ | head -n1 | cut -d: -f1` + 1 )) $^ > .$@
	head -n+$$(( `grep -n ^$$ .$@ | head -n1 | cut -d: -f1` - 1 )) .$@ > $@
	-printf ',s/        //g\n,s/^.$$//g\nw\nq\n' | ed $@
	$(RM) .$@

dist/doc/html/LudumDare25/index.html: dist/setup-config Main.hs SomeMap.hs Types.hs
	-cabal haddock --hyperlink-source --executables

dist/setup-config: LudumDare25.cabal
	cabal configure

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)
	$(RM) -r dist dist-ghc Derive.hs

Derive.hs: Types.hs
	derive -d Lens -iTypes -iData.Lens.Common -m Derive Types.hs > Derive.hs

dist/build/LudumDare25/LudumDare25: LudumDare25.cabal dist/setup-config Main.hs Derive.hs Types.hs SomeMap.hs
	cabal build --ghc-options="$(GHCFLAGS)"

dist/LudumDare25-$(VERSION).tar.gz: LudumDare25.cabal dist/setup-config README Main.hs Derive.hs Types.hs SomeMap.hs
	cabal check
	cabal sdist
