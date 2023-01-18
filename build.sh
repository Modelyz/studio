#!/bin/bash
HERE=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
pushd $HERE

set -e
mkdir -p build
pushd $( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

front/build.sh -o
back/build.sh -o

popd
