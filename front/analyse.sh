#!/bin/bash

while true; do
    inotifywait -r -e modify -e create -e delete src/*elm src/*/*elm || killall less &
    sleep 0.1
    npx elm-analyse | grep -v INFO: | less -R
done
