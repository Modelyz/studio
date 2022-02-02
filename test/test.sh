#!/bin/bash

../build.sh
pipenv run pytest ./test.py
