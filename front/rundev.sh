#!/bin/bash
HERE=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
pushd $HERE

./build.sh dontstop
# build the index (this is normally done in the docker entrypoint
export WSS=ws://localhost:8080
envsubst < src/index.html > ../build/index.html

npx elm-go -p 8080 -u -d ../build/ -s index.html src/Main.elm -- --output ../build/static/app.js

popd
