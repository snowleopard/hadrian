#!/usr/bin/env bash

# By default on Linux/MacOS we build Hadrian using Cabal
(. "hadrian/build.cabal.sh" "$@")
