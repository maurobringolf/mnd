SHELL := /bin/bash

# PHONY targets
.PHONY: build doc test autoformat coverage

build:
	dune build

coverage:
	rm . -name '*.coverage' | xargs rm -f
	dune runtest --instrument-with bisect_ppx --force
	bisect-ppx-report html
	bisect-ppx-report summary

docs:
	dune build @doc

publish-docs:
	dune build @doc
	if [ -z "`git status --porcelain`" ]; then\
		{ git checkout docs; cp -r _build/default/_doc/_html/mnd/* ./mnd/; git add .; git commit -m "Update docs"; git push; git checkout main; };\
	else\
		echo "Cannot publish docs: Repository not clean"; fi

test:
	dune runtest -f

autoformat:
	dune build @fmt --auto-promote

clean:
	dune clean
