#!/bin/bash

if [ "$1" == "-c" ]; then
    while true; do
        inotifywait -r -e modify -e create -e delete src/*elm src/*/*elm src/static/* || killall less &
        sleep 0.1
	rsync -r --delete src/static/ ../build/
        unbuffer elm make --output ../build/app.js src/Main.elm 2>&1 | grep -v Compiling... | less -R
    done
elif [ "$1" == "-o" ]; then
    rsync -r --delete src/static/ ../build/
    elm make --optimize --output ../build/elm.js src/Main.elm \
    && uglifyjs ../build/elm.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output ../build/app.js \
    && rm ../build/elm.js
else
    rsync -r --delete src/static/ ../build/
    elm make --output ../build/app.js src/Main.elm
fi
