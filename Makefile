DUNE ?= dune
OPAM ?= opam

build:
	$(DUNE) build

format fmt:
	$(DUNE) fmt

watch:
	$(DUNE) build -w

_opam:
	$(OPAM) switch create .
	$(OPAM) install ocaml-lsp-server ocamlformat
