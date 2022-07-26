#!/bin/bash

# define studio host in .ssh/config
ssh studio "cd services/modelyz && git pull && /opt/bin/docker-compose build && /opt/bin/docker-compose up -d"
