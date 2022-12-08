#!/bin/bash

pushd ../front
./build.sh
popd
cabal run modelyz-studio -- -d ../build -f ../data/eventstore.txt
