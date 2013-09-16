GHC=ghc --make -O2 -threaded -rtsopts

all: LocalGame

LocalGame: $(wildcard *.hs)
	$(GHC) LocalGame.hs

test: LocalGame Web/drag.html
	./LocalGame web ai &
	x-www-browser Web/drag.html

clean:
	rm -f LocalGame SimpleGame *.hi *.o
