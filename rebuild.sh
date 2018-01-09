#!/bin/bash

echo "Cleaning up"
[ -s libpghaskell.so ] && rm libpghaskell.so
stack clean || exit 1

echo "Building dev version with stack"
stack install --flag pghaskell-internal:debug --flag pghaskell:debug || exit 1
ln -s `stack path --local-install-root`/lib/libpghaskell.so . || exit 1
echo "Done"

