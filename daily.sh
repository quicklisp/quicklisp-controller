#!/bin/bash

cd `dirname $0`
PATH=$PATH:/usr/local/bin
screen -c daily.screenrc -dmS daily-build
