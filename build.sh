#!/bin/bash

if [ "$1" == "-c" ]; then
    while true; do
        inotifywait -r -e modify -e create -e delete *elm */*elm || killall less &
        sleep 0.1
        unbuffer elm make --output static/app.js Main.elm 2>&1 | grep -v Compiling... | less -R
    done
elif [ "$1" == "-o" ]; then
    elm make --optimize --output elm.js Main.elm \
    && uglifyjs elm.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output static/app.js \
    && rm elm.js
else
    elm make --output static/app.js Main.elm
fi
