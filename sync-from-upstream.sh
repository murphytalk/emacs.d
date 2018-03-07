#!/usr/bin/env bash
if [[ $USER == "murphy" ]];then
    SYNC_TO=master
else
    SYNC_TO=for_upstream_merge
fi

git show-branch $SYNC_TO > /dev/null 
if [ $? -ne 0 ];then
    echo Cannot find local branch $SYNC_TO !
    exit 1
fi

git fetch upstream
git checkout $SYNC_TO

git merge upstream/master
