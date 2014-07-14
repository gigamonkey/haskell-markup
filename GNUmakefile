sources = $(wildcard *.hs)
binaries = $(basename $(sources))

GHC_OPTS := -O2
GHC_OPTS += -XNoMonomorphismRestriction

all: $(binaries)

check: tests
	./tests

tests: tests.hs markup.hs

lint:
	/Users/peter/Library/Haskell/bin/hlint $(sources)

clean: tidy
	rm -f $(binaries)
	rm -f *.hi
	rm -f *.o

tidy:
	rm -f *~

%: %.hs
	ghc $(GHC_OPTS) $<
