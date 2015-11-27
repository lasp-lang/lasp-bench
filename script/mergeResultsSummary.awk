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
    print "elapsed, window, total, successful, failed"
    for (l = 2; l <= max; l++) {
	for (i = 1; i <= 5; i++) {
	    if(i != 1)
		printf ", "
	    if(i == 1 || i == 2) {
		a[l, i] = a[l, i] / filecount
	    }
	    # switch (i) {
	    # case 1:
	    # 	a[l, i] = a[l, i] / filecount
	    # 	break
	    # case 2:
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
