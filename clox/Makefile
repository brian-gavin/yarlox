CC := gcc
CFLAGS := -std=c99 

CFILES :=  $(wildcard *c)

all:
	$(CC) $(CFLAGS) -o clox $(CFILES)

clean:
	rm $(OFILES)

.PHONY: clean

