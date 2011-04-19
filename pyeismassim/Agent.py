# TODO:
# - make sure the disconnect method playes nice with the server, i.e. sends the appropriate message so server does not say premature end of file.
# - fix behaviour in case the server dies unexpectedly on the agent, make sure he cleans up and dies gracefully.

import sys
import time
from timeit import Timer
from connection.MASSimConnection import MASSimConnection
from connection.MessageHandling import *


HOST = "127.0.0.1"
PORT = 12300
USER = "a1"
PASS = "1"

log = open('xml.txt', 'w')
# Connect and authenticate.
connection = MASSimConnection()
connection.connect(HOST, PORT, USER, PASS)

# Receive simulation start notification.
print "@Agent: Waiting for simulation start notification."
xml = connection.receive()
print xml
log.write(xml)
msg = parse(xml)

print "@Agent: received:"
print_message(msg)

steps = int(msg['steps'])
#for step in range(steps):
for step in range(1,11):
    print "@Agent: step", step
    xml = connection.receive()
    log.write(xml)
    msg = parse(xml)	
    print_message(msg)
    action_id = msg['id']
    print "Action id:", action_id
    connection.send(action(action_id, "skip"))

log.close()

# Send bye message and disconnect.
connection.send(bye())
connection.disconnect()
