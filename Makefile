all:
	ocamlbuild -cflag -safe-string -use-ocamlfind src/coucoubot.native

clean:
	ocamlbuild -clean

.PHONY: all clean
