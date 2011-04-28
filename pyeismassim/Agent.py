# TODO:
# - make sure the disconnect method playes nice with the server, i.e. sends the appropriate message so server does not say premature end of file.
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
    msg = parse(xml)
    log.write(xml)

    print "@Agent: received:"
    print_message(msg)

    quit = False
    while (not quit):
        xml = connection.receive()
        log.write(xml)
        msg = parse(xml)
        print_message(msg)
        if (msg['type'] == 'bye'):
            quit = True
        elif (msg['type'] == 'request_action'):
            action_id = msg['id']
            print "Action id:", action_id
            action_xml = action(action_id, "skip")
            log.write(action_xml)
            connection.send(action_xml)

    log.close()
    connection.disconnect()
