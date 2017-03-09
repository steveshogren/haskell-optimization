T :
	@echo auto-build
	@echo auto-serve
	@echo watchify

auto-build :
	stack build --file-watch

auto-serve :
	. auto-serve.sh

watchify :
	sudo npm run watchify
