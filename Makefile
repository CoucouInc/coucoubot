all: build test

build:
	@dune build @install

test: build
	@dune runtest --no-buffer --force

watch:
	@dune build @install -w

clean:
	@dune clean

doc:
	@dune build @doc


backups:
	#@echo "doing backups of all .json files…"
	./tools/save.sh *.json

.PHONY: backups
