#!/bin/bash

BUILD="cabal build"
OPTIMIZE=" -O2 --enable-split-objs --enable-executable-stripping --enable-library-stripping --enable-executable-dynamic"
DIR="dist-newstyle/build/x86_64-linux/ghc-8.8.4/ms-0.1.0.0/x/ms/"

if [ "$1" == "-c" ]; then
    while true; do
        inotifywait -r -e modify -e create -e delete src/*hs src/*/*hs || killall less &
        sleep 0.1
	$BUILD
	cp -a $EXEC ../build/
    done
elif [ "$1" == "-o" ]; then
    $BUILD $OPTIMIZE
    EXEC="$DIR/opt/build/ms/ms"
    cp -af $EXEC ../build/
else
    $BUILD
    EXEC="$DIR/build/ms/ms"
    cp -af $EXEC ../build/
fi
