sources = $(wildcard *.hs)
binaries = $(basename $(sources))

all: $(binaries)

check: tests
	./tests

lint:
	/Users/peter/Library/Haskell/bin/hlint $(sources)

clean: tidy
	rm -f $(binaries)
	rm -f *.hi
	rm -f *.o

tidy:
	rm -f *~

%: %.hs
	ghc -O2 $<
