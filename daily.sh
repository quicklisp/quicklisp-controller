#!/bin/bash

cd `dirname $0`
PATH=$PATH:/usr/local/bin
screen -L -c daily.screenrc -dmLS daily-build
