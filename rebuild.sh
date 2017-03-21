#!/bin/sh

rm libpghaskell.so
stack clean
stack build --flag pghaskell:-dummy
ln -sf pghaskell/.stack-work/dist/x86_64-linux/Cabal-1.24.2.0/build/libHSpghaskell-*.so libpghaskell.so
