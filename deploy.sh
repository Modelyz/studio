#!/bin/bash

if [ "$1" == "--reset" ]; then
    echo "Deleting data"
    RESET="/opt/bin/docker-compose down -v"
elif [ "$1" == "--keep" ]; then
    RESET="echo keeping data"
else
    echo use --reset to delete data or --keep to keep it
    exit 1
fi

# define studio host in .ssh/config
ssh studio "cd services/modelyz && $RESET && git pull && /opt/bin/docker-compose build && /opt/bin/docker-compose up -d"
