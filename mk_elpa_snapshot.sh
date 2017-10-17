#!/usr/bin/env bash
ELPA="`dirname $0`/elpa"

if [ ! -d $ELPA ];then
	echo Fold $ELPA does not exist!
else
    tar cfj elpa-`date +%Y%m%d`.tar.bz2 --exclude "*.elc" elpa
fi
