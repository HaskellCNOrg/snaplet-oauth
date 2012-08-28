
HC=ghc
DIST=dist
CDEV=cabal-dev

default: build

init:
	cabal update
	$(CDEV) install

clean:
	rm -rf $(DIST)

conf:
	$(CDEV) configure

conf-test:
	$(CDEV) configure --enable-tests

build: conf
	$(CDEV) build

build-test: conf-test
	$(CDEV) build

rebuild: clean build

install: build
	$(CDEV) install

test: build-test
	$(CDEV) test

test-demo:
	cd test/ && runghc -package-conf=../cabal-dev/packages-7.4.1.conf/ snap.hs -b 127.0.0.1 -p 9988

local:
	rm -rf ./cabal-dev/lib/hoauth*
	rm -f ./cabal-dev/packages-7.4.1.conf/hoauth2-*
	$(CDEV) add-source ../hoauth2
	$(CDEV) install
