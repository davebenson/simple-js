all: jast.o js-string.o jhash.o tests/test-jast

mk-scan-utf8-character-class: mk-scan-utf8-character-class.c
	gcc -W -Wall -o $@ $^

generated/scan-id-continue.c: mk-scan-utf8-character-class character-classes/id-continue.txt
	@mkdir -p generated
	./mk-scan-utf8-character-class character-classes/id-continue.txt > generated/scan-id-continue.c
generated/scan-id-start.c: mk-scan-utf8-character-class character-classes/id-start.txt
	@mkdir -p generated
	./mk-scan-utf8-character-class character-classes/id-start.txt > generated/scan-id-start.c

CFLAGS = -W -Wall -g
clean:
	rm -f *.o
	rm tests/test-jast generated/*.c mk-scan-utf8-character-class

%.o: %.c
	gcc -c $(CFLAGS) $<

jutf8.o: generated/scan-id-continue.c generated/scan-id-start.c jutf8.c
	gcc $(CFLAGS) -c jutf8.c

tests/test-jast: jast.o js-string.o jhash.o jutf8.o tests/test-jast.c
	gcc $(CFLAGS) -o tests/test-jast jast.o js-string.o jhash.o jutf8.o tests/test-jast.c

