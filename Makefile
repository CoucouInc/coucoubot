all: coucoubot.ml
	ocamlbuild -cflag -safe-string -use-ocamlfind coucoubot.native

clean:
	ocamlbuild -clean
