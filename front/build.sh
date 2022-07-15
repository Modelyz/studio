#!/bin/bash

set -e
pushd $( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

export IDBVERSION=18 ### IndexedDB version. Upgrade when a json format change occurs ###

if [[ -z "${WSS}" ]]; then
    export WSS="ws://localhost:8080"
fi

if [ "$1" == "-o" ]; then
    rsync -r --delete src/static/ ../build/static/
    envsubst < src/index.html > ../build/index.html
    elm make --optimize --output ../build/tmp.js src/Main.elm \
        && uglifyjs ../build/tmp.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
        | uglifyjs --mangle --output ../build/static/app.js \
    && rm ../build/tmp.js
else
    rsync -r --delete src/static/ ../build/static/
    envsubst < src/index.html > ../build/index.html
    elm make --output ../build/static/app.js src/Main.elm
fi
popd
