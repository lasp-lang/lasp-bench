set term png 
set output "summary.png"
set title "Throughput"
set xtics ("99.99" 1, "90" 2, "25" 3, "50" 4, "1" 5)
set tics out
#set logscale y
set xlabel "Read (Percentage)"
set ylabel "Operations/Sec"
plot "plots" using 1:2 title 'Total' with linespoints, \
"plots" using 1:3 title 'Successful' with linespoints, \
"plots" using 1:4 title 'Error' with linespoints
