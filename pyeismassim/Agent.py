# -*- coding: utf-8 -*-

# TODO:
# - fix behaviour in case the server dies unexpectedly on the agent, make sure he cleans up and dies gracefully.

import sys
import time
from timeit import Timer
from connection.MASSimConnection import MASSimConnection
from connection.MessageHandling import *

if (__name__== "__main__"):
    HOST = "127.0.0.1"
    PORT = 12300
    if (len(sys.argv) == 3):
        USER = sys.argv[1]
        PASS = sys.argv[2]
    else:
        print "Usage: python Agent.py USERNAME PASSWORD"
        sys.exit()

    log = open('xml.txt', 'w')
    # Connect and authenticate.
    connection = MASSimConnection()
    connection.connect(HOST, PORT, USER, PASS)

    # Receive simulation start notification.
    print "@Agent: Waiting for simulation start notification."
    xml = connection.receive()
    _, _, msg = parse(xml, 'dict')
    log.write(xml)

    print "@Agent: received:"
    print_message(msg, 'dict')

    quit = False
    while (not quit):
        xml = connection.receive()
        log.write(xml)
        msg_type, action_id, msg = parse(xml, 'prolog')
        log.write(str(msg))
        #sys.stdin.read(1)
        if (msg_type == 'request-action'):
            print "@Agent: received request-action. id:", action_id
            action_xml = action(action_id, "skip")
            log.write(action_xml)
            print_message(msg, 'prolog') # DEBUG
            connection.send(action_xml)
        elif (msg_type == 'bye'):
            print "@Agent: received bye"
            quit = True
        elif (msg_type == 'sim-end'):
            print "@Agent: received sim-end"
            quit = True
        else:
            print "@Agent: en area 51"

    log.close()
    connection.disconnect()
