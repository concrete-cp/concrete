#!/bin/sh

java -Xmx640M -XX:MaxPermSize=128M -ea -cp .:options.jar:jel.jar cspfj.Cspfj $@