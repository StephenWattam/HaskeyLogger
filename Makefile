
CC=ghc
CFLAGS=-O -Wall -fno-warn-missing-signatures
#-fwarn-incomplete-patterns 
#-v
EXE=sc

all:
	$(CC) $(CFLAGS) -o $(EXE) --make Main

test: all
	sudo ./$(EXE)

clean:
	rm -rfv *.o *.hi $(EXE)

