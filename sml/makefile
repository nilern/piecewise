.PHONY: default
default: target/pcws

target/pcws: target src/main.mlb \
	         src/util/option_ext.sml src/util/list_ext.sml src/util/vector_ext.sml \
			 src/util/pprint.sml \
			 src/pos.sml src/ir/name.sml src/ir/var.sml src/ir/const.sml src/ir/primop.sml \
			 src/ir/expr.sml src/ir/cstmt.sml src/ir/cst.sml \
			 src/ir/dnf.sml src/ir/augless_stmt.sml src/ir/ast.sml \
			 src/ir/desugar_binds.sml \
			 src/ir/bind_stmt1.sml src/ir/augless_ast.sml \
			 src/ir/desugar_augs.sml \
			 src/ir/flat_expr.sml src/ir/flat_ast.sml \
			 src/ir/convert_lenv.sml src/ir/convert_denv.sml \
			 src/parser.grm.sig src/parser.grm.sml src/lexer.lex.sml \
			 src/main.sml
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
