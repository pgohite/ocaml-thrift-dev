FLAGS=-use-menhir -tag thread -use-ocamlfind -quiet -pkg core

all: main

main: othrift_main.ml
	corebuild $(FLAGS) othrift_main.native

clean:
	rm -rf _build *.native
