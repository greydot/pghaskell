#!/bin/sh

rm libpghaskell.so
stack clean
#stack build --flag pghaskell:-dummy --flag pghaskell:debug --flag pghaskell-internal:debug
stack build --flag pghaskell:dummy --flag pghaskell:debug --flag pghaskell-internal:debug
ln -sf pghaskell/.stack-work/dist/x86_64-linux/Cabal-1.24.2.0/build/libHSpghaskell-*.so libpghaskell.so
