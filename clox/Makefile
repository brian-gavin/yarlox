CC := gcc

DEBUG_FLAGS ?= -D DEBUG_TRACE_EXECUTION -D DEBUG_PRINT_CODE -D DEBUG_STRESS_GC -D DEBUG_LOG_GC

CFLAGS := -std=c99

CFILES :=  $(wildcard *c)
OFILES := clox

debug:
	$(CC) $(CFLAGS) $(DEBUG_FLAGS) -o clox $(CFILES)

all:
	$(CC) $(CFLAGS) -o clox $(CFILES)

clean:
	rm $(OFILES)

.PHONY: all clean

