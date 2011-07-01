import socket
import sys
 
HOST = 'localhost'
PORT = 10000
ADDR = (HOST,PORT)
BUFSIZE = 4096
  
if (__name__ == "__main__"):
    if   (sys.argv[1] == '1'):
        percepts = [
                {"1" : "a", "2" : "b", "3" : "c"},
                {"4" : "d", "5" : "e", "6" : "f"},
                {"7" : "g", "8" : "h", "9" : "i"}
                ]
    elif (sys.argv[1] == '2'):
        percepts = [
                {"4" : "d", "5" : "e", "6" : "f"},
                {"7" : "g", "8" : "h", "9" : "i"},
                {"1" : "a", "2" : "b", "3" : "c"}
                ]
    elif (sys.argv[1] == '3'):
        percepts = [
                {"7" : "g", "8" : "h", "9" : "i"},
                {"1" : "a", "2" : "b", "3" : "c"},
                {"4" : "d", "5" : "e", "6" : "f"}
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

