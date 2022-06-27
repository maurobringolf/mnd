SHELL := /bin/bash

.PHONY: build
build:
	dune build

.PHONY: coverage
coverage:
	rm . -name '*.coverage' | xargs rm -f
	dune runtest --instrument-with bisect_ppx --force
	bisect-ppx-report html
	bisect-ppx-report summary

.PHONY: docs
docs:
	dune build @doc

.PHONY: publish-docs
publish-docs:
	dune build @doc
	if [ -z "`git status --porcelain`" ]; then\
		{ git checkout docs; cp -r _build/default/_doc/_html/mnd/* ./mnd/; git add .; git commit -m "Update docs"; git push; git checkout main; };\
	else\
		echo "Cannot publish docs: Repository not clean"; fi

.PHONY: test
test:
	dune runtest -f

.PHONY: autoformat
autoformat:
	dune build @fmt --auto-promote

.PHONY: clean
clean:
	dune clean
