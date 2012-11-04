import socket
import sys
import PerceptServer
 
if (__name__ == "__main__"):
    HOST = 'localhost'
    PORT = 10000
    ADDR = (HOST,PORT)
      
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
    connection = PerceptServer.PerceptConnection(HOST, PORT)
    connection.connect()

    for i in range(3):
        print "Iteration:", i

        # send data
        connection.send(percepts[i])

        # receive data
        percept_difference = connection.recv()

        print "Client had:", percepts[i]
        print "Client received:", percept_difference

    # close socket
    connection.disconnect()

