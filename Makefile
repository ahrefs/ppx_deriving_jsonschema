.DEFAULT_GOAL := help

DUNE = opam exec -- dune

.PHONY: help
help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'

.PHONY: build
build: ## Build the project
	$(DUNE) build

.PHONY: test
test: ## Run tests
	$(DUNE) runtest

.PHONY: test-promote
test-promote: ## Run tests and promote expected outputs
	$(DUNE) runtest --auto-promote

.PHONY: clean
clean: ## Clean build artifacts
	$(DUNE) clean

.PHONY: fmt
fmt: ## Format code with ocamlformat
	$(DUNE) build @fmt --auto-promote

.PHONY: fmt-check
fmt-check: ## Check formatting without modifying files
	$(DUNE) build @fmt

.PHONY: doc
doc: ## Build documentation
	$(DUNE) build @doc

.PHONY: doc-open
doc-open: doc ## Build and open documentation
	open _build/default/_doc/_html/index.html 2>/dev/null || xdg-open _build/default/_doc/_html/index.html

.PHONY: install
install: ## Install the package
	$(DUNE) install

.PHONY: uninstall
uninstall: ## Uninstall the package
	$(DUNE) uninstall

.PHONY: init
init: ## Set up a development environment from scratch
	opam init --bare --shell-setup --yes
	opam switch create . --deps-only --with-test --with-dev-setup --yes
	opam install . --deps-only --with-test --with-dev-setup --yes

.PHONY: deps
deps: ## Install development dependencies
	opam install . --deps-only --with-test --with-dev-setup --yes

.PHONY: opam-lint
opam-lint: ## Lint the opam file
	opam lint ppx_deriving_jsonschema.opam

.PHONY: all
all: build test fmt-check opam-lint ## Build, test, check formatting, and lint
