#!/bin/bash

INSTALLDIR=../build
set -e

pushd $( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
mkdir -p ../build

if [ "$1" == "-o" ]; then
    # TODO stripping does not work
    cabal install -O2 --enable-split-objs --enable-executable-static --enable-executable-stripping --install-method=copy --installdir=$INSTALLDIR --overwrite-policy=always
    #cabal v1-install -O2 --enable-split-objs --enable-executable-static --enable-executable-stripping --bindir=$INSTALLDIR
else
    cabal install --ghc-options="-Wall" --installdir=$INSTALLDIR --overwrite-policy=always
fi
popd
