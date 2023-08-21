#!/bin/sh

# suppression de tous les fichiers vides (taille=0) dans /repA
cd /data_cdt/SATELLITE_DATA/model_data/tamsat3.1/nc/
cd /data_cdt/SATELLITE_DATA/model_data/chirps/tiff/Afrique/
find ./ -size 0 -exec rm -f {} \;
