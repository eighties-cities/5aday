#!/bin/bash

curl --cookie-jar cookie.txt https://zenodo.org/api/deposit/depositions/4835981?access_token=UgRS8QIaWT18o4nUDrIvG0oOVrugRO1WzATH5WEGRNVZCt6BB2bzyEyIm3eZ > data.json

DOWNLOAD_URL=$(jq '.files[].links.download' data.json)

#echo ${DOWNLOAD_URL}

#DOWLOAD_CLEAN_URL=$(sed 's|\r||' <<< "${DOWNLOAD_URL}")
DOWLOAD_CLEAN_URL=$( "${DOWNLOAD_URL}" | sed 's|\r||')


echo "${DOWNLOAD_CLEAN_URL}"

#curl --cookie cookie.txt "${DOWNLOAD_URL}"



# clef UgRS8QIaWT18o4nUDrIvG0oOVrugRO1WzATH5WEGRNVZCt6BB2bzyEyIm3eZ
