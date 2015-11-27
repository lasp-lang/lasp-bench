#!/bin/awk -f
BEGIN {
    FS=","
    max = 0
    filecount = 0
}
FNR == 1 {
    filecount++
}
FNR!=1 {
    if (FNR > max)
	max = FNR
    for (i = 1; i <= NF; i++) {
	a[FNR, i] = $i + a[FNR, i]
    }
}
END {
    print "elapsed, window, n, min, mean, median, 95th, 99th, 99_9th, max, errors"
    for (l = 2; l <= max; l++) {
	for (i = 1; i <= 11; i++) {
	    if(i != 1)
		printf ", "
	    if(i >=1 && i <= 10) {
		a[l, i] = a[l, i] / filecount
	    }
	    # switch (i) {
	    # case 1:
	    # 	a[l, i] = a[l, i] / filecount
	    # 	break
	    # case 2:
	    # 	a[l, i] = a[l, i] / filecount
	    # 	break
	    # case 4:
	    # 	a[l, i] = a[l, i] / filecount
	    # 	break
	    # case 5:
	    # 	a[l, i] = a[l, i] / filecount
	    # 	break
	    # case 6:
	    # 	a[l, i] = a[l, i] / filecount
	    # 	break
	    # case 7:
	    # 	a[l, i] = a[l, i] / filecount
	    # 	break
	    # case 8:
	    # 	a[l, i] = a[l, i] / filecount
	    # 	break
	    # case 9:
	    # 	a[l, i] = a[l, i] / filecount
	    # 	break
	    # case 10:
	    # 	a[l, i] = a[l, i] / filecount
	    # 	break
	    # default:
	    # 	break
	    # }
	    printf "%s",a[l, i]
	}
	print ""
    }
}
#awk 'FNR==NR{a[$1]=$2 FS $3;next}{ print $0, a[$1]}'
