#!/bin/bash

./build.sh dontstop
npx elm-go -p 8080 -u -d ../build/ -s index.html src/Main.elm -- --output ../build/static/app.js
