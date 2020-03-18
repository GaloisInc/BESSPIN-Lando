#!/bin/bash

read INPUT
CMD="ps -lextext \"$INPUT\" | parse -lang=InfoLeakageEng | linearize -lang=InfoLeakageClafer"
DIR=`dirname $0`
echo $CMD | gf --run $DIR/InfoLeakageEng.gf $DIR/InfoLeakageClafer.gf
