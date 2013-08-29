GHC=ghc --make -O2 -threaded -rtsopts

all: LocalGame

LocalGame: $(wildcard *.hs)
	$(GHC) LocalGame.hs

clean:
	rm -f LocalGame SimpleGame *.hi *.o
