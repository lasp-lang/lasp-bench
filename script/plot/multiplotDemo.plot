set multiplot layout 2,2 rowsfirst

#set term png 
# set output "results-pubsub_weak_meta_data-2dcs-4nodes-2benchNodes/write_latencies.png"
 set title "Update Latencies"
#set xtics ("99.99(.01)" 1, "99(1)" 2, "90(10)" 3, "75(25)" 4, "50(50)" 5, "1(99)" 6)
#set xrange [0:10]
set tics out
set logscale y
#set key font ",18"
#set tics font ", 18"
set xlabel "Elapsed time (10s per tic)"
set ylabel "Latency (ms)"
plot "writeDemoValues" using 1:2 title 'Minimum' with linespoints, \
"writeDemoValues" using 1:3 title 'Mean' with linespoints, \
"writeDemoValues" using 1:4 title 'Median' with linespoints, \
"writeDemoValues" using 1:5 title '95th' with linespoints, \
"writeDemoValues" using 1:6 title '99th' with linespoints, \
"writeDemoValues" using 1:7 title '99_9th' with linespoints, \
"writeDemoValues" using 1:8 title 'Max' with linespoints

#set term png 
# set output "results-pubsub_weak_meta_data-2dcs-4nodes-2benchNodes/write_latencies.png"
 set title "Read Latencies"
#set xtics ("99.99(.01)" 1, "99(1)" 2, "90(10)" 3, "75(25)" 4, "50(50)" 5, "1(99)" 6)
#set xrange [0:10]
set tics out
set logscale y
#set key font ",18"
#set tics font ", 38"
set xlabel "Elapsed time (10s per tic)"
set ylabel "Latency (ms)"
plot "readDemoValues" using 1:2 title 'Minimum' with linespoints, \
"readDemoValues" using 1:3 title 'Mean' with linespoints, \
"readDemoValues" using 1:4 title 'Median' with linespoints, \
"readDemoValues" using 1:5 title '95th' with linespoints, \
"readDemoValues" using 1:6 title '99th' with linespoints, \
"readDemoValues" using 1:7 title '99_9th' with linespoints, \
"readDemoValues" using 1:8 title 'Max' with linespoints

#set term png 
#set output "summary_demo.png"
 set title "Throughput"
#set xtics ("99.99(.01)" 1, "99(1)" 2, "90(10)" 3, "75(25)" 4, "50(50)" 5, "1(99)" 6)
#set xtics rotate by 90 right
#set xrange [0:10]
set tics out
set nologscale y
#set key font ", 18"
#set tics font ", 18"
set xlabel "Elapsed time (10s per tic)"
set ylabel "Operations/Second"
plot "summaryDemoValues" using 1:2 title 'Total' with linespoints, \
"summaryDemoValues" using 1:3 title 'Successful' with linespoints, \
"summaryDemoValues" using 1:4 title 'Error' with linespoints


pause 10
reread
