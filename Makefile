all: jast.o js-string.o jhash.o tests/test-jast

clean:
	rm -f *.o

%.o: %.c
	gcc -c -W -Wall $<

tests/test-jast: jast.o js-string.o jhash.o tests/test-jast.c
	gcc -W -Wall -o tests/test-jast jast.o js-string.o jhash.o tests/test-jast.c
