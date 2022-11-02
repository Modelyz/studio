#!/bin/bash

./build.sh dontstop
npx elm-go -u -d ../build/ -s index.html src/Main.elm -- --output ../build/static/app.js
