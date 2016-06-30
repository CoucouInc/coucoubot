TARGETS = src/coucoubot.native src/coucoulib.cma src/coucoulib.cmxa src/coucoubot.cmxs

all:
	ocamlbuild -cflag -safe-string -use-ocamlfind $(TARGETS)

clean:
	ocamlbuild -clean

.PHONY: all clean
