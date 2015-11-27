#set term png 
# set output "results-pubsub_weak_meta_data-2dcs-4nodes-2benchNodes/write_latencies.png"
 set title "Read Latencies"
#set xtics ("99.99(.01)" 1, "99(1)" 2, "90(10)" 3, "75(25)" 4, "50(50)" 5, "1(99)" 6)
set xrange [0:10]
set tics out
set logscale y
set xlabel "Elapsed time (10s per tic)"
set ylabel "Latency (ms)"
plot "readDemoValues" using 1:2 title 'Minimum' with linespoints, \
"readDemoValues" using 1:3 title 'Mean' with linespoints, \
"readDemoValues" using 1:4 title 'Median' with linespoints, \
"readDemoValues" using 1:5 title '95th' with linespoints, \
"readDemoValues" using 1:6 title '99th' with linespoints, \
"readDemoValues" using 1:7 title '99_9th' with linespoints, \
"readDemoValues" using 1:8 title 'Max' with linespoints

pause 10
reread

#sed -i "1i read_ratio, min, mean, median, 95th, 99th, 99_9th, max" readPlot
