#!/bin/bash

pushd $( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

front/build.sh
back/build.sh

popd
