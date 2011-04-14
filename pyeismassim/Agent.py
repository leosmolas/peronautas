# TODO:
# - make sure the disconnect method playes nice with the server, i.e. sends the appropriate message so server does not say premature end of file.
# - implement the rest of the methods facilitating sending of actions
# - make the connection run in a thread, in which each continuously receives percepts

import sys
import time
from MASSimConnection import MASSimConnection

HOST = "127.0.0.1"
PORT = 12300
USER = "a1"
PASS = "1"

connection = MASSimConnection()
connection.connect(HOST, PORT, USER, PASS)
while True:
    print "@Agent: Starting to receive connection"
    msg = connection.receive()
    print "@Agent: received: '%s'" % msg
    sys.stdin.readline()

connection.disconnect()
