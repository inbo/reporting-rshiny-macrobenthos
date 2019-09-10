#!/bin/bash
# Make a tar.gz from the R-package from the code
cd /home/ubuntu/reporting-rshiny-macrobenthos
if [ -f reporting-macrobenthos.tar.gz ]; then
    rm reporting-macrobenthos.tar.gz
fi
tar -zcvf reporting-macrobenthos.tar.gz macrobenthosapp
# Build the docker image
sudo docker build --no-cache -t IMAGENAMEHERE .