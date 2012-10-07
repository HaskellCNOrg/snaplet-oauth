
HC=ghc
DIST=dist
CDEV=cabal-dev
STYLE=stylish-haskell


.PHONY: clean test hlint

default: build

init:
	cabal update
	$(CDEV) install

clean:
	rm -rf $(DIST)
	rm -rf ./cabal-dev/lib/snaplet-oauth*
	rm -rf ./cabal-dev/packages/snaplet-oauth*
	rm -f ./cabal-dev/packages-7.4.1.conf/snaplet-oauth-*

hlint:
	hlint src/ tests/ --report=$(DIST)/hlint.html
	find src tests example -name '*.hs' | xargs $(STYLE) -i 

conf:
	$(CDEV) configure --enable-tests

build: conf
	$(CDEV) build

rebuild: clean build

install: build
	$(CDEV) install --enable-tests

reinstall: clean install

test: build
	$(CDEV) test

demo:
	cd example && make clean prev

doc:
	$(CDEV) haddock

dist: clean build
	$(CDEV) sdist

### build before push in case compilation error

push: clean build
	git push --all

fun:
	cloc src
	cloc tests

local:
	rm -rf ./cabal-dev/lib/hoauth2*
	rm -rf ./cabal-dev/packages/hoauth2*
	rm -f ./cabal-dev/packages-7.4.1.conf/hoauth2-*
	$(CDEV) add-source ../hoauth2
	$(CDEV) install

## 
## cd example/ && runghc -package-conf=../cabal-dev/packages-7.4.1.conf/ snap.hs -b 127.0.0.1 -p 9988
