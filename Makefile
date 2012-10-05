
HC=ghc
DIST=dist
CDEV=cabal-dev

.PHONY: clean test

default: build

init:
	cabal update
	$(CDEV) install

init-keys:
	cp data/Weibo.Key.hs src/Snap/Snaplet/OAuth/Weibo/Key.hs

clean:
	rm -rf $(DIST)
	rm -rf ./cabal-dev/lib/snaplet-oauth*
	rm -rf ./cabal-dev/packages/snaplet-oauth*
	rm -f ./cabal-dev/packages-7.4.1.conf/snaplet-oauth-*

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

demo:
	cd example && make clean prev


local:
	rm -rf ./cabal-dev/lib/hoauth*
	rm -rf ./cabal-dev/packages/hoauth*
	rm -f ./cabal-dev/packages-7.4.1.conf/hoauth2-*
	$(CDEV) add-source ../hoauth2
	$(CDEV) install

## 
## cd example/ && runghc -package-conf=../cabal-dev/packages-7.4.1.conf/ snap.hs -b 127.0.0.1 -p 9988
