#!/bin/bash

# define studio host in .ssh/config
ssh studio "cd services/studio && git pull && /opt/bin/docker-compose build && /opt/bin/docker-compose up -d"
