

HC=ghc

DIST=dist

default: build

clean:
	rm -rf $(DIST)

conf:
	cabal configure

conf-test:
	cabal configure --enable-tests

build: conf
	cabal build

build-test: conf-test
	cabal build

rebuild: clean build

install: build
	cabal install

test: build-test
	cabal test

test-demo:
	cd test/ && runghc snap.hs -b 127.0.0.1 -p 9988

