#!/bin/zsh

for i in $@
do
rm solution
./cspfj.sh -solver cspfj.solver.MACSolver -d FINE -competition $i | tail -n1 >| solution
cat solution
java -jar ../lib/solutionChecker.jar $i solution
done
