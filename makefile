.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


.PHONY: test
test:
	stack test


.PHONY: install
install:
	stack install


.PHONY: docs
docs:
	stack haddock --haddock-for-hackage


.PHONY: release
release: docs
	stack upload .
	stack upload --documentation .


.PHONY: format
format:
	fourmolu -i src/**/*.hs app/*.hs


.PHONY: clean
clean:
	stack clean --full
	rm -rf dist-newstyle
