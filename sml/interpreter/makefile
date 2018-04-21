.PHONY: default
default: target/pcws

.PHONY: clean
clean:
	rm -f src/lexer.lex.sml
	rm -f src/parser.grm.*
	rm -rf target

target/pcws: target src/main.mlb \
	           $(shell find src -name '*.sml' -or -name '*.fun' -or -name '*.sig') \
			       src/lexer.lex.sml src/parser.grm.sig src/parser.grm.sml
	mlton -const 'Exn.keepHistory true' -output target/pcws src/main.mlb

src/lexer.lex.sml: src/lexer.lex
	mllex src/lexer.lex

src/parser.grm.sig: src/parser.grm
	mlyacc src/parser.grm

src/parser.grm.sml: src/parser.grm
	mlyacc src/parser.grm

target:
	mkdir -p target
