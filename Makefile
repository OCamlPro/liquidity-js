set-mode:
ifeq ($(MODE),)
ifeq ($(wildcard RELEASE),)
    MODE := development
else
    MODE := production
endif
endif

all: dist/liquidity.node.js dist/liquidity.browser.js

_obuild/liquidity-js/liquidity-js.js: _obuild submodules
	@ocp-build build liquidity-js

dist/liquidity.node.js: node_modules _obuild/liquidity-js/liquidity-js.js wrapper/liquidity-js-wrapper.js
	@echo --- Building NodeJS library -------------------------
	@npx webpack --mode $(MODE) --target node wrapper/liquidity-js-wrapper.js -o $@

dist/liquidity.browser.js: node_modules _obuild/liquidity-js/liquidity-js.js wrapper/liquidity-js-wrapper.js
	@echo --- Building browser library ------------------------
	@npx webpack --mode $(MODE) --target web wrapper/liquidity-js-wrapper.js -o $@

publish-npm: release-file dist/liquidity.node.js
	@rm -f RELEASE
	@cp -f package.json package.json.bak
	sed -i s/0.0.0-dev/$$( \
	X=$$(grep "version =" libs/liquidity/tools/liquidity/build.ocp2 | cut -d '"' -f 2); \
	M=$$(echo $$X | cut -d '.' -f1); \
	Y=$$(echo $$X | cut -d '.' -f2); \
	N=$$(echo $$Y | cut -d '-' -f1 | cut -c1-1); \
	P=$$(echo $$Y | cut -d '-' -f1 | cut -c2-99); \
	R=$$(echo $$Y | cut -sd '-' -f2); \
	[ -z $$N ] && N=0; \
	[ -z $$P ] && P=0; \
	[ -z $$R ] || R=-$$R; \
	echo "$$M.$$N.$$P$$R" \
	)/ package.json
	npm publish --dry-run
	@rm -f package.json
	@mv package.json.bak package.json

submodules-update: .gitmodules
	git submodule update --init --recursive

libs/liquidity libs/ocp-libsoduim-js libs/ocplib-ezjsonm-js: submodules-update

submodules: libs/liquidity libs/ocp-libsoduim-js libs/ocplib-ezjsonm-js

_obuild: Makefile _opam
	ocp-build init
_opam:
	opam switch create . 4.07.1 --no-install

node_modules: package.json
	@npm install --only=dev

build-deps: _opam opam node_modules
	make -C libs/liquidity build-deps
	opam install . --deps-only --working-dir -y

clean: _obuild
	ocp-build clean

distclean: clean
	rm -rf _obuild

release-file:
	touch RELEASE
	$(eval MODE := production)

release: release-file all
	@rm -f RELEASE
