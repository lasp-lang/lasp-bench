set term png 
set output "results/overall-summary.png"
set title "Throughput"
set xtics ("99.99(.01)" 1, "90(10)" 2, "25(75)" 3, "50(50)" 4, "1(99)" 5)
#set xtics rotate by 90 right
set tics out
#set logscale y
set xlabel "Percentage of Read(Update) Operations"
set ylabel "Operations/Second"
plot "plots" using 1:2 title 'Total' with linespoints, \
"plots" using 1:3 title 'Successful' with linespoints, \
"plots" using 1:4 title 'Error' with linespoints
