#!/bin/bash

npx elm-go -u -d ../build/ -s index.html src/Main.elm -- --output ../build/static/app.js
