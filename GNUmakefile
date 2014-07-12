source = $(wildcard *.hs)
binaries = $(basename $(source))

all: $(binaries)

test: $(binaries)
	./markup

lint:
	hlint $(source)

clean: tidy
	rm -f $(binaries)
	rm -f *.hi
	rm -f *.o

tidy:
	rm -f *~

%: %.hs
	ghc -O2 $<
