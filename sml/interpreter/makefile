.PHONY: default
default: target/pcws

.PHONY: clean
clean:
	rm -f src/pass/lexer.lex.sml
	rm -f src/pass/parser.grm.*
	rm -rf target

target/pcws: target src/main.mlb \
	           $(shell find src -name '*.sml' -or -name '*.fun' -or -name '*.sig')
	mlton -const 'Exn.keepHistory true' -output target/pcws src/main.mlb

target:
	mkdir -p target
