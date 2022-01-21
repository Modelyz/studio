#!/bin/bash

BUILD="cabal build"
OPTIMIZE=" -O2 --enable-split-objs --enable-executable-stripping --enable-library-stripping --enable-executable-dynamic"

if [ "$1" == "-c" ]; then
    while true; do
        inotifywait -r -e modify -e create -e delete src/*hs src/*/*hs || killall less &
        sleep 0.1
	$BUILD
    done
elif [ "$1" == "-o" ]; then
    $BUILD $OPTIMIZE
else
    $BUILD
fi
