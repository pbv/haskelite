

SRC = src/AST.elm src/HsParser.elm src/Indent.elm src/Haskelite.elm src/PrettyPrinter.elm src/Shows.elm src/Machine.elm src/Machine/Types.elm src/Machine/Heap.elm src/Context.elm src/Prelude.elm src/Types.elm src/Typecheck.elm src/Unify.elm src/Tc.elm

JS = site/js/haskelite.js

MINJS = site/js/haskelite-min.js

$(MINJS) : $(JS)
	uglifyjs $(JS) --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output $(MINJS)

$(JS) : $(SRC)
	# elm make src/Haskelite.elm  --output $(JS)
	elm make src/Haskelite.elm --optimize --output $(JS)



