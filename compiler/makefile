.PHONY: default
default: target/pcws

target/pcws: target src/main.mlb \
	         $(shell find src -name '*.sml' -or -name '*.fun' -or -name '*.sig') \
			 src/pass/lexer.lex.sml src/pass/parser.grm.sig src/pass/parser.grm.sml
	mlton -link-opt -no-pie -const 'Exn.keepHistory true' -output target/pcws src/main.mlb

src/pass/parser.grm.sig: src/pass/parser.grm
	mlyacc src/pass/parser.grm

src/pass/parser.grm.sml: src/pass/parser.grm
	mlyacc src/pass/parser.grm

src/pass/lexer.lex.sml: src/pass/lexer.lex
	mllex src/pass/lexer.lex

target:
	mkdir -p target

.PHONY: clean
clean:
	rm -f src/pass/lexer.lex.sml
	rm -f src/pass/parser.grm.*
	rm -rf target
