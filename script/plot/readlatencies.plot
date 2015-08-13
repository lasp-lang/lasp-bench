set term png 
set output "write_latencies.png"
set title "Write Latencies"
set xtics ("99.99" 1, "90" 2, "25" 3, "50" 4, "1" 5)
set tics out
set logscale y
set xlabel "Read (Percentage)"
set ylabel "Latency (ms)"
plot "plots" using 1:2 title 'Minimum' with linespoints, \
"plots" using 1:3 title 'Mean' with linespoints, \
"plots" using 1:4 title 'Median' with linespoints, \
"plots" using 1:5 title '95th' with linespoints, \
"plots" using 1:6 title '99th' with linespoints, \
"plots" using 1:7 title '99_9th' with linespoints, \
"plots" using 1:8 title 'Max' with linespoints


#sed -i "1i read_ratio, min, mean, median, 95th, 99th, 99_9th, max" readPlot