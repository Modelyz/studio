#!/bin/bash

set -e
BUILD="cabal build"
DEVEL='--ghc-options="-Wall"'
OPTIMIZE=" -O2 --enable-split-objs --enable-executable-stripping --enable-library-stripping --enable-executable-dynamic"
DIR="dist-newstyle/build/x86_64-linux/"

pushd $( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

if [ "$1" == "-o" ]; then
    $BUILD $OPTIMIZE
    find $DIR -type f -name studio -exec cp {} ../build/ \;
else
    $BUILD $DEVEL
    find $DIR -type f -name studio -exec cp {} ../build/ \;
fi
popd
