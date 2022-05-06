#!/bin/bash

npx elm-go -v -d ../build/ -s index.html src/Main.elm -- --output ../build/static/app.js
