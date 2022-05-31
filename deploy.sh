#!/bin/bash


if [ "$1" == "--build" ]; then
   build="/opt/bin/docker-compose build"
else
    build="echo \"(skipping build)\""
fi

# define studio host in .ssh/config
ssh studio "cd services/studio && git pull && $build && /opt/bin/docker-compose up -d"
