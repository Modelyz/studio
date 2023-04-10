#!/bin/bash
HERE=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
pushd $HERE

./build.sh

npx elm-go -p 8080 -u -d ../build/ -s index.html src/Main.elm -- --output ../build/static/app.js

popd
