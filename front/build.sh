#!/bin/bash

pushd $( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

if [ "$1" == "-o" ]; then
    rsync -r --delete src/static/ ../build/static/
    elm make --optimize --output ../build/tmp.js src/Main.elm \
        && uglifyjs ../build/tmp.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
        | uglifyjs --mangle --output ../build/static/app.js \
    && rm ../build/tmp.js
else
    rsync -r --delete src/static/ ../build/static/
    elm make --output ../build/static/app.js src/Main.elm
fi
popd
