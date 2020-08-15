help: ## Print documentation
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

build: ## Build the app
	nix-build release.nix

test: ## Test the app
	nix-shell \
		--run "script/watch --command \"cabal repl marble-os-test\""

update-deps: ## Update the deps from the .cabal file to marble-os.nix
	nix-shell \
		--pure -p cabal2nix --run "cabal2nix ." > marble-os.nix

repl: ## Repl
	nix-shell \
		--run "cabal new-repl"

.PHONY: serve build test update-deps repl