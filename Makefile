
.PHONY: all
all:
	@echo "nothing to do"


.PHONY: test
test:
	@runghc -itest/driver test/driver/Main.hs test
