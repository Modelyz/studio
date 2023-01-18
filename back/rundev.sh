#!/bin/bash
HERE=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
pushd $HERE

pushd ../front
./build.sh
popd
cabal run modelyz-studio -- -d ../build -f ../data/eventstore.txt

popd
