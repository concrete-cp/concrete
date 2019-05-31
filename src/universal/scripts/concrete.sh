#!/bin/bash

export JAVA_HOME=/usr/lib/jvm/java-11-openjdk-amd64

export JAVA_OPTS="-Xss16M -Xmx4G -server"

limit=1000s
kill=60s

concrete=$HOME/git/concrete/target/universal/stage
conf=$concrete/conf

if [[ $@ == *.xml* ]]
then
  	main=xcsp-3-concrete
elif [[ $@ == *.fzn* ]]
then
	main=fz-concrete
fi

timeout -k $kill $limit $concrete/bin/$main -Dconfig.file=$conf/application.conf -Dlogback.configurationFile=$conf/logback.xml $@
