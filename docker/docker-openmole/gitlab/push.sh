#!/bin/bash

docker build . -t openmole/build
docker push openmole/build

