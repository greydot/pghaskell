#!/bin/bash

echo "Cleaning up"
rm libpghaskell.so
stack clean || exit 1

echo "Building dev version with stack"
stack install --flag pghaskell-internal:debug --flag pghaskell:debug || exit 1
ln -s `stack path --local-install-root`/lib/libpghaskell.so . || exit 
echo "Done"

