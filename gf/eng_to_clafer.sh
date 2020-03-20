#!/bin/bash

tmp_dir=$(mktemp -d -t eng_to_clafer_XXXXXXX)

read INPUT
CMD="ps -lextext \"$INPUT\" | parse -lang=InfoLeakageEng | linearize -lang=InfoLeakageClafer"
DIR=`dirname $0`
echo $CMD | gf --gfo-dir=${tmp_dir} --run $DIR/InfoLeakageEng.gf $DIR/InfoLeakageClafer.gf

rm -rf $tmp_dir
