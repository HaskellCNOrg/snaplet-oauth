
DIST=dist
CDEV=cabal-dev --sandbox=../cabal-dev
TARGET=./dist/build/example/example

.PHONY: example

default: build

init:
	cabal update
	$(CDEV) install

clean:
	rm -rf $(DIST)

conf:
	$(CDEV) configure --flags="development"

build: conf
	$(CDEV) build

rebuild: clean build

install: build
	$(CDEV) install

prev: build
	$(TARGET) -p 9988

#	cd example/ && runghc -package-conf=../cabal-dev/packages-7.4.1.conf/ snap.hs -b 127.0.0.1 -p 9988
