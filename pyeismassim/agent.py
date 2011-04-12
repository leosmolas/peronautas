import sys
import time
from serverConnection import serverConnection

HOST = "127.0.0.1"
PORT = 12300
USER = "a1"
PASS = "1"

connection = serverConnection()
connection.connect(HOST, PORT)
time.sleep(2)
connection.authenticate(USER, PASS)
#while True:
#    msg = connection.receive()
#    print "@Agent: received: '%s'" % msg
#    sys.stdin.readline()
