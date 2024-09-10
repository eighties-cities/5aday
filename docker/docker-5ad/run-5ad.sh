#!/bin/bash

docker build . --tag h24 --no-cache --build-arg UID="$(id -u)" --build-arg GID="$(id -g)"

docker run --name h24 -e UID=$(id -u $USER) -e GID=$(id -g $USER) -v "$PWD/data:/home/h24/source/prepared_data_IDF" h24:latest "runMain eighties.h24.tools.ExtractRelevantData -c data/CONTOURS-IRIS_2-0__SHP_LAMB93_FXX_2014-01-01/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2014/CONTOURS-IRIS_2-0_SHP_LAMB93_FE-2014/CONTOURS-IRIS_FE.shp -g data/GRID/R_rfl09_LAEA1000.shp -p data/base-ic-evol-struct-pop-2012.xls -f data/base-ic-diplomes-formation-2012.xls -d 75,77,78,91,92,93,94,95 -o prepared_data_IDF"

