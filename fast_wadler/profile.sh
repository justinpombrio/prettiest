#!/bin/bash

ghc -O2 -prof -fprof-auto -rtsopts --make RunPretty && ./RunPretty +RTS -p -RTS run "$@"
