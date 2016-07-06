TARGETS = src/coucoubot.native src/coucoulib.cma src/coucoulib.cmxa src/coucoubot.cmxs

atd:
	atdgen -t src/movie.atd
	atdgen -j -j-std src/movie.atd

all: atd
	ocamlbuild -cflag -safe-string -use-ocamlfind $(TARGETS)

clean:
	rm src/movie_t.ml*
	rm src/movie_j.ml*
	ocamlbuild -clean

.DEFAULT_GOAL := all
.PHONY: all clean
