# Lasp Bench (former Basho Bench) [![Build Status](https://travis-ci.org/lasp-lang/lasp-bench.svg?branch=master)](https://travis-ci.org/lasp-lang/lasp-bench)
A load-generation and testing tool for basically whatever you can write a returning Erlang function for.  
Lasp Bench picks up where Basho Bench left off, offering the same functionality under current versions of Erlang/OTP and build tools. It is available as a Hex package.

## Overview

Basho Bench is a benchmarking tool created to conduct accurate and
   repeatable performance tests and stress tests, and produce
   performance graphs.

   Originally developed to benchmark Riak, it exposes a pluggable
   driver interface and has been extended to serve as a benchmarking
   tool across a variety of projects.

   Basho Bench focuses on two metrics of performance:

   - Throughput: number of operations performed in a timeframe,
     captured in aggregate across all operation types
   - Latency: time to complete single operations, captured in
     quantiles per-operation

## Quick Start

   You must have [Erlang/OTP 18.3][1] or later to build and run Basho
   Bench, and [R][2] to generate graphs of your benchmarks.  A sane
   GNU-style build system is also required if you want to use `make`
   to build the project.

```bash
git clone git://github.com/lasp-lang/lasp-bench.git
cd lasp_bench
make all
```

This will build an executable script, `lasp_bench`, which you can
use to run one of the existing benchmark configurations from the
`examples/` directory. You will likely have to make some minor directory
changes to the configs in order to get the examples running (see, e.g., the
source of the bitcask and innostore benchmark config files for direction).

At the end of the benchmark, results will be available in CSV
format in the =tests/current/= directory. Now you can generate a
graph:

```bash
make results
priv/summary.r -i tests/current
Loading required package: proto
Loading required package: reshape
Loading required package: plyr
Loading required package: digest
null device
          1
$ open tests/current/summary.png
```

## Troubleshooting Graph Generation

If make results fails with the error `/usr/bin/env: Rscript --vanilla: No such file or directory`
please edit priv/summary.r and replace the first line with the full path to the Rscript binary on your system

If you receive the error message `Warning: unable to access index for repository http://lib.stat.cmu.edu/R/CRAN/src/contrib`
it means the default R repo for installing additional packages is broken, you can change it as follows:

```bash
$ R
> chooseCRANmirror()
Selection: 69
quit()
make results
```

## Customizing your Benchmark
Basho Bench has multiple drivers, each with its own configuration, and
a number of key and value generators that you can use to customize
your benchmark. It is also straightforward -- with less than 200
lines of Erlang code -- to create custom drivers that can exercise
other systems or perform custom operations. These are covered more
in detail in the [[http://docs.basho.com/riak/latest/cookbooks/Benchmarking/][documentation]].

## Alternative Graph Generation by gnuplot
You can generate graphs using gnuplot.

```bash
$ ./priv/gp_throughput.sh
$ ./priv/gp_latency.sh
```

By passing `-h` option to each script, help messages are shown.

Some of options for these scripts are:

- `-d TEST_DIR` : comma separated list of directories which include
     test result CSV files
- `-t TERMINAL_TYPE` : gnuplot terminal type
- `-P` : just print gnuplot script without drawing graph

For example, you can draw graphs with ASCII characters
by the option =-t dumb=, which is useful in non-graphical
environment or quick sharing of result in chat.

Also, you can plot multiple test runs on a single plot by using `-d` switch.

[1]: http://erlang.org/download.html
[2]: http://www.r-project.org
