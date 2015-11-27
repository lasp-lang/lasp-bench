#!/usr/bin/env python3

import sys
import zmq

context = zmq.Context()

if len(sys.argv) <= 1:
    print("Usage: antidote_intercept.py PUBLISHER_IP_PORT")
    exit(0)

address = sys.argv[1]
subscriber = context.socket(zmq.SUB)
subscriber.connect("tcp://" + address)
subscriber.setsockopt(zmq.SUBSCRIBE, b'')

while True:
    msg = subscriber.recv()
    partition_id_bytes = msg[:20]
    partition_id_int = int.from_bytes(partition_id_bytes, byteorder='big')
    print("TXN from ", address, ", partition " + str(partition_id_int))
