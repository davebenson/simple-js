all: jast.o tests/test-jast

clean:
	rm -f *.o

jast.o: jast.c
	gcc -c -W -Wall jast.c

tests/test-jast: jast.o tests/test-jast.c
	gcc -W -Wall -o tests/test-jast jast.o tests/test-jast.c
