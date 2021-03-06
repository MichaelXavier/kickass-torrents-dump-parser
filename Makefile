CABAL=cabal-dev
EXPORTS=PATH=$$PATH:cabal-dev/bin
CONFIG_OPTS=

all: build

build: configure src/**/*.hs
	$(EXPORTS) $(CABAL) build

install: build
	cabal install

uninstall:
	ghc-pkg unregister kickass-torrents-dump-parser


sdist: configure
	$(CABAL) sdist
	
configure: kickass-torrents-dump-parser.cabal install_dependencies
	$(CABAL) configure $(CONFIG_OPTS)

install_dependencies:
	$(CABAL) install --only-dependencies

test: configure_tests
	PATH=$$PATH:cabal-dev/bin $(CABAL) build 
	$(CABAL) test

configure_tests:
	$(CABAL) configure --enable-tests $(CONFIG_OPTS)

docs:
	$(CABAL) haddock

clean:
	$(CABAL) clean
	rm -f **/*.{o,hi} **/**/*.{o,hi}
