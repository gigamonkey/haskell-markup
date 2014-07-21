sources = $(wildcard *.hs)
binaries = $(basename $(sources))

GHC_OPTS := -O2
GHC_OPTS += -W
GHC_OPTS += -XNoMonomorphismRestriction

all: $(binaries)

check: test test_files
	./test
	./test_files tests/*.txt

test: test.hs markup.hs

test_files: test_files.hs markup.hs

parse_test: parse_test.hs markup.hs

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
