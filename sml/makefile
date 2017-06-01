.PHONY: default
default: target/pcws

target/pcws: target src/main.mlb src/main.sml \
	         src/lexer.lex.sml src/parser.grm.sig src/parser.grm.sml
	mlton -output target/pcws src/main.mlb

src/parser.grm.sig: src/parser.grm
	mlyacc src/parser.grm

src/parser.grm.sml: src/parser.grm
	mlyacc src/parser.grm

src/lexer.lex.sml: src/lexer.lex
	mllex src/lexer.lex

target:
	mkdir -p target

.PHONY: clean
clean:
	rm -f src/lexer.lex.sml
	rm -f src/parser.grm.*
	rm -rf target
