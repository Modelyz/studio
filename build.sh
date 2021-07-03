#!/bin/bash

while true; do
    inotifywait -r -e modify -e create -e delete *elm */*elm || killall most &
    sleep 0.1
    unbuffer elm make --output static/app.js Main.elm 2>&1 | grep -v Compiling... | less -R
done
