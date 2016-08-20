
.PHONY: all
all:
	@rm -f bin/tree
	@cd packages/datum-tree; cabal configure; cabal build
	@cd bin; ln -s ../packages/datum-tree/dist/build/datum-tree/datum-tree tree

.PHONY: clean
clean:
	@cd packages/datum-tree; cabal clean

.PHONY: test
test:
	@runghc -itest/driver test/driver/Main.hs test
