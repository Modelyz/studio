#!/bin/bash

set -e

export APPVERSION=0
export IDBVERSION=23 ### IndexedDB version. Upgrade when a json format change occurs ###

if [[ -z "${WSS}" ]]; then
    export WSS="ws://localhost:8080"
fi

pushd $( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
grep "## version ${APPVERSION} --" ../CHANGELOG.md \
    || { echo "Please first feed the changelog for version ${APPVERSION}"; exit 1; }

rsync -r --delete src/static/ ../build/static/

if [ "$1" == "-o" ]; then
    elm make --optimize --output ../build/tmp.js src/Main.elm \
        && uglifyjs ../build/tmp.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
        | uglifyjs --mangle --output ../build/static/app.js \
    && rm ../build/tmp.js
    export APP=app.${APPVERSION}.$( md5sum ../build/static/app.js | cut -d' ' -f1 )
    mv ../build/static/app.js ../build/static/${APP}.js
else
    elm make --output ../build/static/app.js src/Main.elm
    export APP=app
fi

envsubst < src/index.html > ../build/index.html

markdown ../CHANGELOG.md > ../build/static/changelog.html

popd
