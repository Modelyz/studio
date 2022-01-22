#!/bin/bash

pushd front
./build.sh $1
popd

pushd back
./build.sh $1
popd
