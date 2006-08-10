#!/bin/sh

for i in $@ ; do
if [ -s $i.ub ] ; then
echo skip $i 
else
echo $i
java -Xmx640M -cp .:options.jar openshop.OpenShop -d INFO $i 1000 1500 > $i.ub 
fi
done
