#!/bin/bash

ghc -O2 --make RunPretty && ./RunPretty run "$@"
