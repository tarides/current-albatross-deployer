.DEFAULT_GOAL := all

ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
$(eval $(ARGS):;@:)

LOCK=current-albatross-deployer.opam.locked

.PHONY: all
all:
	dune build --root . @install

.PHONY: deps
deps: _opam ${LOCK} ## Install development dependencies
	opam install -y ocamlformat merlin
	opam monorepo pull

_opam: ## Create an opam switch without any dependency
	opam switch create . --empty -y
	opam install ocaml dune

.PHONY: lock
lock: ${LOCK} ## Generate a lock file
	opam lock -y .

.PHONY: start
start: all ## Run the produced executable
	dune exec -- bin/main.exe $(ARGS)

.PHONY: test
test: ## Run the unit tests
	dune runtest

.PHONY: clean
clean: ## Clean build artifacts and other generated files
	dune clean

.PHONY: doc
doc: ## Generate odoc documentation
	dune build @doc

.PHONY: servedoc
servedoc: doc ## Open odoc documentation with default web browser
	open _build/default/_doc/_html/index.html

.PHONY: fmt
fmt: ## Format the codebase with ocamlformat
	dune build --auto-promote @fmt

.PHONY: watch
watch: ## Watch for the filesystem and rebuild on every change
	dune build --watch

.PHONY: utop
utop: ## Run a REPL and link with the project's libraries
	dune utop lib -- -implicit-bindings
