#!/bin/bash

set -e 
./build.sh
npx elm-go -u -d ../build/ -s index.html src/Main.elm -- --output ../build/static/app.js
