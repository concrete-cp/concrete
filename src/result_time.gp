#set terminal postscript eps "Times-Roman" 18

set key bottom left

set pointsize 1

set xlabel "CPU time"
set ylabel "% unsolved instances"

set logscale xy

set output "selection.ps"



plot [.1:300] \
"bqwh.dat" using 2:1 with steps title "Combo", \
"bqwh.dat" using 3:1 with steps title "MAC"

pause -1