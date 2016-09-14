CFLAGS=-O2 -std=c99

a.out: main.c tasks.o tasks.h utils.h
	gcc $(CFLAGS) -fopenmp main.c tasks.o

tasks.o: utils.h tasks.c tasks.h

clean:
	rm -rf *.o a.out
