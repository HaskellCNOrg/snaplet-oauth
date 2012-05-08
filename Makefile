

HC=ghc

DIST=dist

default: build

clean:
	rm -rf $(DIST)

conf:
	cabal configure

build: conf
	cabal build

rebuild: clean build

install: build
	cabal install

test-weibo:
	cd test/Weibo && runghc snap.hs -b 127.0.0.1 -p 9988

#test-weibo:
#	runghc test/Weibo/snap.hs -b 127.0.0.1 -p 9988
