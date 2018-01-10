#!/bin/bash

PATH="$HOME/.local/bin:$PATH"

echo "Cleaning up"
[ -s libpghaskell.so ] && rm libpghaskell.so
stack clean || exit 1

echo "Building dev version with stack"
stack install --flag pghaskell-internal:debug --flag pghaskell:debug || exit 1
#ln -s `stack path --local-install-root`/lib/libpghaskell.so . || exit 1

echo "Buiding test version with cabal"
for d in pghaskell-internal pghaskell; do
    cd $d
    cabal install -fdebug --force-reinstalls
    rm -r dist
    cd ..
done
ln -s ~/.cabal/lib/libpghaskell.so .
echo "Done"
