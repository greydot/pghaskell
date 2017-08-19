#!/bin/bash

rm libpghaskell.so
echo
echo "Building dev version with stack"
echo
stack build --flag pghaskell-internal:debug --flag pghaskell:debug || exit 1

echo
echo "Buiding test version with cabal"
echo
for d in pghaskell-internal pghaskell; do
    cd $d
    cabal install -fdebug --force-reinstalls
    rm -r dist
    cd ..
done
ln -sf ~/.cabal/lib/x86_64-linux-ghc-8.0.2/libHSpghaskell-0.1-*.so libpghaskell.so
