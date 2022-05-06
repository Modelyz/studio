#!/bin/bash

BUILD="cabal build"
DEVEL='--ghc-options="-Wall"'
OPTIMIZE=" -O2 --enable-split-objs --enable-executable-stripping --enable-library-stripping --enable-executable-dynamic"
DIR="dist-newstyle/build/x86_64-linux/ghc-8.8.4/ms-0.1.0.0/x/ms/"

pushd $( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

if [ "$1" == "-c" ]; then
    while true; do
        inotifywait -e modify -e create -e delete src/*hs src/*/*hs || killall less &
        sleep 0.25
    $BUILD
    cp -a $EXEC ../build/
    done
elif [ "$1" == "-o" ]; then
    $BUILD $OPTIMIZE
    EXEC="$DIR/opt/build/ms/ms"
    cp -af $EXEC ../build/
else
    $BUILD $DEVEL
    EXEC="$DIR/build/ms/ms"
    cp -af $EXEC ../build/
fi
popd
