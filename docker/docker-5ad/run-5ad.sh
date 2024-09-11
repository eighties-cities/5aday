#!/bin/bash

docker build . --tag 5ad --no-cache --build-arg UID="$(id -u)" --build-arg GID="$(id -g)"

docker run -it -d --name 5ad -e UID=$(id -u $USER) -e GID=$(id -g $USER) --mount type=bind,source=$(dirname "$PWD")/docker-h24/result/,target=/home/5ad/source/prepared_data_IDF --mount type=bind,source=$(dirname "$PWD")/docker-h24/result/jar,target=/home/5ad/.ivy2 5ad:latest

docker exec 5ad sh -c "sbt -J-Xmx2G 'runMain eighties.fiveaday.run.SimulationApp -d ./data/initialisation_distribution_per_cat_2002_2008.csv -p prepared_data_IDF/population.bin -m prepared_data_IDF/moves.bin'"
