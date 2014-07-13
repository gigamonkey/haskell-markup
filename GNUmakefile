sources = $(wildcard *.hs)
binaries = $(basename $(sources))

all: $(binaries)

test: $(binaries)
	./markup

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
