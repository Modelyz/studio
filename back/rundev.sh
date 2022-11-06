#!/bin/bash

pushd ../front
./build.sh dontstop
popd
cabal run modelyz-studio -- -d ../build -f ../data/eventstore.txt
