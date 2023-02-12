

SRC = src/AST.elm src/HsParser.elm src/Haskelite.elm src/Pretty.elm src/Eval.elm src/Context.elm src/Prelude.elm src/Typecheck.elm src/Unify.elm src/Tc.elm

JS = haskelite.js

MINJS = haskelite-min.js

$(MINJS) : $(JS)
	uglifyjs $(JS) --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output $(MINJS)

$(JS) : $(SRC)
	#elm make src/Haskelite.elm  --output $(JS)
	elm make src/Haskelite.elm --optimize --output $(JS)

