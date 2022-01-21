#!/bin/bash

while true; do
    inotifywait -r -e modify -e create -e delete *elm */*elm || killall less &
    sleep 0.1
    ./node_modules/.bin/elm-analyse | grep -v INFO: | less -R
done
