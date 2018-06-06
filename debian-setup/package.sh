#!/bin/bash


mkdir -p done
for f in $(cat debian-9-packages.txt);do
    if ! [ -f done/$f ];then
       sudo apt-get -y install $f
       touch done/$f
    fi
done
