FLAGS=-use-menhir -tag thread -use-ocamlfind -quiet -pkg core
GENS=othrift_exp.ml generator.mli cthrift_gen.ml
all: main

main: othrift_main.ml
	corebuild $(FLAGS) othrift_main.native
gen: $(GENS)
	corebuild $(GENS) cthrift_gen.native

clean:
	rm -rf _build *.native
