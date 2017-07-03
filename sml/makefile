.PHONY: default
default: target/pcws

target/pcws: target src/main.mlb src/main.sml \
	         src/util/option_ext.sml src/primop.sml src/pos.sml \
			 src/ir/const.sml src/ir/var.sml src/ir/name.sml src/ir/cst0.sml \
	         src/parser.grm.sig src/parser.grm.sml src/lexer.lex.sml \
			 src/util/pprint.sml src/util/vector_ext.sml src/ir/flat_cst.sml \
			 src/ir/stmt0.sml src/ir/expr0.sml src/ir/expr1.sml \
			 src/ir/lex_flatten.sml src/ir/dnf.sml src/ir/dnf_cst.sml \
			 src/ir/stmt1.sml src/ir/pat_expand.sml \
			 src/ir/bind_stmt1.sml src/ir/augless_cst.sml \
			 src/ir/desugar_augs.sml
	mlton -const 'Exn.keepHistory true' -output target/pcws src/main.mlb

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
