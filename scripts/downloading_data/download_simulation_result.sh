#!/bin/bash

curl --cookie-jar cookie.txt https://zenodo.org/api/deposit/depositions/13711644?access_token=UgRS8QIaWT18o4nUDrIvG0oOVrugRO1WzATH5WEGRNVZCt6BB2bzyEyIm3eZ > info.json
DOWNLOAD_URL=$(jq -r '.links.files' info.json)

curl --cookie cookie.txt ${DOWNLOAD_URL} > info2.json
DOWNLOAD_FILES_URL=$(jq -r '.entries[].links.content' info2.json)

#DOWLOAD_CLEAN_URL=$(sed 's|\r||' <<< $DOWNLOAD_URL )
#echo "${DOWNLOAD_CLEAN_URL}"

curl --cookie cookie.txt ${DOWNLOAD_FILES_URL} --output HigherProp.zip

unzip HigherProp.zip

# clef UgRS8QIaWT18o4nUDrIvG0oOVrugRO1WzATH5WEGRNVZCt6BB2bzyEyIm3eZ
