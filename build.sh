#!/bin/bash
HERE=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
pushd $HERE

set -e
mkdir -p build

if [ "$1" == "-o" ]; then
    front/build.sh -o
    back/build.sh -o
else
    front/build.sh
    back/build.sh
fi

popd
