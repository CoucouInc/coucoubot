all: build test

build:
	@dune build @install

build-docker:
	@docker build . -f Dockerfile.server -t coucoubot:latest

test: build
	@dune runtest --no-buffer --force

watch:
	@dune build @install -w

format:
	@dune build $(DUNE_OPTS) @fmt --auto-promote

format-check:
	@dune build $(DUNE_OPTS) @fmt --display=quiet

clean:
	@dune clean

doc:
	@dune build @doc


backups:
	#@echo "doing backups of all .json files…"
	./tools/save.sh coucoubot.db

.PHONY: backups
