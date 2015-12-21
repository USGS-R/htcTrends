#!/bin/bash
export PATH=/usr/local/bin:/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/sbin

chmod 777 unzip
./unzip packages.zip

mkdir rLibs

Rscript $1 $2


