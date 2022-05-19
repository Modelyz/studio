#!/bin/bash


if [ "$1" == "--build" ]; then
   build="--build"
else
    build=""
fi

# define studio host in .ssh/config
ssh studio "cd services/studio && git pull && /opt/bin/docker-compose up -d $build"
