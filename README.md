# paragon-optimization

## First Build

* Install [stack](https://docs.haskellstack.org/en/stable/README/)
* Clone repo locally
* First time from repo root run `stack setup` then `stack build` to make sure
  everything works
* Optional for Linux: install `inotify-tools`

## Running the site on a loop

`stack build --file-watch` will force a rebuild on file changes.

If you have inotify-tools installed, you can have the site live start on a loop
by running: `. test.sh` in a different tab.

Otherwise: `stack exec paragon-optimization` will start the site, you will
have to kill it manually with `Ctrl-C` each time and restart it.

## Adding Libraries

* Ideally only use libraries from [stackage](https://www.stackage.org/)
* Add new libraries to `paragon-optimization.cabal`
* Run `stack build` to pull down deps and ensure they compile

## Scotty 

* Some Scotty [basics](http://seanhess.github.io/2015/08/19/practical-haskell-json-api.html)

