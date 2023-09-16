#!/bin/bash
HERE=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
pushd $HERE

set -e
mkdir -p build

pushd back
which cabal-cache && cabal-cache sync-from-archive --archive-uri ~/.cabal/archive
popd

if [ "$1" == "-o" ]; then
    front/build.sh -o
    back/build.sh -o
else
    front/build.sh
    back/build.sh
fi

pushd back
which cabal-cache && cabal-cache sync-to-archive --archive-uri ~/.cabal/archive
popd

popd
