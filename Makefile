PROJECT := randomlang
LINK_PKG := pgocaml
COMP_PKG := pgocaml,pgocaml.syntax

all: $(PROJECT)

$(PROJECT): $(PROJECT).ml
	corebuild randomlang.native

