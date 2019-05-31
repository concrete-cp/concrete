#!/bin/sh

while :
do
	pending=$(psql -h iutinfo-sgbd -U concrete -tAF\   < fetch.sql)
	if [ "" = "$pending" ]
	then
	    break
	fi
	eval "./concrete.sh -sql $pending"
done
