all: jast.o tests/test-jast

mk-scan-utf8-character-class: mk-scan-utf8-character-class.c
	gcc -W -Wall -o $@ $^

generated/scan-id-continue.c: mk-scan-utf8-character-class character-classes/id-continue.txt
	@mkdir -p generated
	./mk-scan-utf8-character-class character-classes/id-continue.txt > generated/scan-id-continue.c

clean:
	rm -f *.o

jast.o: jast.c
	gcc -c -W -Wall jast.c

tests/test-jast: jast.o tests/test-jast.c
	gcc -W -Wall -o tests/test-jast jast.o tests/test-jast.c

jutf8.o: generated/scan-id-continue.c jutf8.c
	gcc -c -W -Wall jutf8.c
