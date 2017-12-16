TARGETS = src/coucoubot.native src/coucoulib.cma src/coucoulib.cmxa src/coucoubot.cmxs src/tools/convert_json.native

all:
	ocamlbuild -cflag -safe-string -use-ocamlfind $(TARGETS)

clean:
	ocamlbuild -clean

backups:
	#@echo "doing backups of all .json files…"
	./tools/save.sh *.json

.DEFAULT_GOAL := all
.PHONY: all clean backups
