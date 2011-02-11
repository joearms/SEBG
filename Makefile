.SUFFIXES: .erl .beam .yrl

MODS := $(wildcard *.erl)

%.beam: %.erl
	erlc  -W $< 

all: beam
	(sleep 1 && ./openchrome http://localhost:1234/index.html) &
	erl -s sebg start 1234 





beam: ${MODS:%.erl=%.beam}

clean:
	rm -rf *.beam 





