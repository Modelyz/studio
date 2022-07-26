#!/bin/bash

RESET="echo keeping data"
if [ "$1" == "--reset" ]; then
    echo "Deleting data"
    RESET="/opt/bin/docker-compose down -v"
fi

# define studio host in .ssh/config
ssh studio "cd services/modelyz && $RESET && git pull && /opt/bin/docker-compose build && /opt/bin/docker-compose up -d"
