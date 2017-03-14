#!/usr/bin/env bash

set -e # Any subsequent(*) commands which fail will cause the shell script
       # to exit immediately
set -x # print each command before executing

####################################################################################################
# Benchmark configuration
####################################################################################################

# basho_bench configuration file to use
if [ -z ${CONFIG_FILE_PATH} ]; then
    CONFIG_FILE_PATH="./examples/antidote_pb.config"
fi

# number of concurrent threads for each benchmark
# ex. usage CONCURRENT_CONFIG="20 40" : produces a set of bench with 20 threads and another with 40
if [ -z ${CONCURRENT_CONFIG} ]; then
    CONCURRENT_CONFIG="40"
fi

# number of keys for each benchmark
if [ -z ${KEYS_CONFIG} ]; then
    KEYS_CONFIG="10000 100000"
fi

# number of updates configuration for each basho_bench
if [ -z ${NUM_UPDATES_CONFIG} ]; then
    NUM_UPDATES_CONFIG="2 10"
fi

### Source & Results directories :
RUN_PATH="$(pwd)"

# basho_bench source directory path
if [ -z ${BASHO_BENCH_PATH} ]; then
    BASHO_BENCH_PATH="$(pwd)"
fi

# antidote source directory path
if [ -z ${ANTIDOTE_PATH} ]; then
    ANTIDOTE_PATH="$BASHO_BENCH_PATH/../antidote"
fi
STALENESS_LOG_PATH="$ANTIDOTE_PATH/_build/default/rel/antidote/data"

# results directory path
if [ -z ${STALENESS_RESULTS_PATH} ]; then
    STALENESS_RESULTS_PATH="$BASHO_BENCH_PATH/../staleness_results"
fi

####################################################################################################
# Benchmark setup
####################################################################################################
if [ ! -d ${STALENESS_RESULTS_PATH} ]
then
    mkdir ${STALENESS_RESULTS_PATH}
fi

# antidote setup
cd ${ANTIDOTE_PATH}
git checkout Physics-protocol-stable
git pull
./rebar3 upgrade

# basho_bench setup
cd ${BASHO_BENCH_PATH}
git checkout antidote_pb-rebar3-erlang19
git pull
make clean
make

# we will work with a copy of basho_bench config file
TMP_CONFIG_FILE="${CONFIG_FILE_PATH}.tmp"
cp ${CONFIG_FILE_PATH} ${TMP_CONFIG_FILE}

####################################################################################################
# API
####################################################################################################
function run_benchmark {
    BENCH_CONFIG_FILE="$1"
    BENCH_LOG_TARGET="$2"

    # antidote setup
    cd ${ANTIDOTE_PATH}
    make clean relclean distclean
    make rel
    INSTANCE_NAME=antidote ./_build/default/rel/antidote/bin/env foreground &
    ./bin/start_bg_processes.erl 'antidote@127.0.0.1'
    sleep 10 # enough time to open staleness log files

    # run basho_bench
    cd ${BASHO_BENCH_PATH}
    ./_build/default/bin/basho_bench ${BENCH_CONFIG_FILE}

    # store results
    if [ -d ${BENCH_LOG_TARGET} ]; then
        rm -r ${BENCH_LOG_TARGET}
    fi
    mkdir ${BENCH_LOG_TARGET}
    cp -r ${BENCH_CONFIG_FILE} ${BENCH_LOG_TARGET}/
    cp -r ${STALENESS_LOG_PATH}/* ${BENCH_LOG_TARGET}/

    # cleanup
    cd ${ANTIDOTE_PATH}
    make clean relclean distclean
    cd ${BASHO_BENCH_PATH}
}

####################################################################################################
# Process
####################################################################################################
for CONCURRENT_NR in ${CONCURRENT_CONFIG}
do
    sed "s/^{concurrent\, .*/{concurrent\, $CONCURRENT_NR}\./" ${TMP_CONFIG_FILE} > ${TMP_CONFIG_FILE}.bak
    mv ${TMP_CONFIG_FILE}.bak ${TMP_CONFIG_FILE}
    for KEYS_NR in ${KEYS_CONFIG}
    do
        sed "s/^{key_generator\, .*/{key_generator\, {pareto_int\, $KEYS_NR}}\./" ${TMP_CONFIG_FILE} > ${TMP_CONFIG_FILE}.bak
        mv ${TMP_CONFIG_FILE}.bak ${TMP_CONFIG_FILE}
        for NUM_UPDATES_NR in ${NUM_UPDATES_CONFIG}
        do
            sed "s/^{num_updates\, .*/{num_updates\, $NUM_UPDATES_NR}\./" ${TMP_CONFIG_FILE} > ${TMP_CONFIG_FILE}.bak
            mv ${TMP_CONFIG_FILE}.bak ${TMP_CONFIG_FILE}

            BENCH_LOG_TARGET="${STALENESS_RESULTS_PATH}/bench_concurrent_${CONCURRENT_NR}_keys_${KEYS_NR}_num_updates_${NUM_UPDATES_NR}"
            run_benchmark ${TMP_CONFIG_FILE} ${BENCH_LOG_TARGET}
        done
    done
done

####################################################################################################
# Cleanup
####################################################################################################
rm ${TMP_CONFIG_FILE}
cd ${RUN_PATH}

echo "[INFO] Benchmark ended successfully!"


