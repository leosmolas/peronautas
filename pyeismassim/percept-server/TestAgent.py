import socket
import sys
 
HOST = 'localhost'
PORT = 10000
ADDR = (HOST,PORT)
BUFSIZE = 4096
  
if (__name__ == "__main__"):
    if   (sys.argv[1] == '1'):
        percepts = [
                frozenset(["1", "2", "3"]),
                frozenset(["4", "5", "6"]),
                frozenset(["7", "8", "9"])
                ]
    elif (sys.argv[1] == '2'):
        percepts = [
                frozenset(["4", "5", "6"]),
                frozenset(["7", "8", "9"]),
                frozenset(["1", "2", "3"])
                ]
    elif (sys.argv[1] == '3'):
        percepts = [
                frozenset(["7", "8", "9"]),
                frozenset(["1", "2", "3"]),
                frozenset(["4", "5", "6"])
                ]
    else:
        raise Exception

    # open the connection to the server
    clientsocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    clientsocket.connect((ADDR))

    for i in range(3):
        print "Iteration:", i
        # send data
        # careful: the clients must send the server frozensets made from lists of strings, not lists of strings
        clientsocket.send(repr(percepts[i]))

        # receive data
        percept_difference = clientsocket.recv(BUFSIZE)
        print "Client had:", percepts[i]
        print "Client received:", percept_difference

    # close socket
    clientsocket.close()

