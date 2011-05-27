import socket

if (__name__ == "__main__"):

    CONNECTIONS = 3
    HOST = 'localhost'
    PORT = 10000
    ADDR = (HOST, PORT)
    BUFSIZE = 4096

    clientsockets = []

    # accept 10 client sockets
    serversocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    serversocket.bind((ADDR))
    serversocket.listen(CONNECTIONS)

    for i in range(CONNECTIONS):
       clientsocket, address = serversocket.accept()
       print "Server: received a connection. address:", address
       clientsockets.append(clientsocket)
    
    j = 0
    quit = False
    while (not quit):
        print "Iteration:", j
        # initialize the global percept to an empty frozenset
        global_percept = frozenset([])
        percepts       = range(CONNECTIONS)

        for i in range(CONNECTIONS):
            clientsocket   = clientsockets[i] 
            percept        = clientsocket.recv(BUFSIZE)
            percepts[i]    = eval(percept)
            global_percept = global_percept.union(percepts[i])

        for i in range(CONNECTIONS):
            # for each socket, calculate the difference between the accumulator 
            # and the original percept and send the difference
            socket     = clientsockets[i]
            percept    = percepts[i]
            difference = global_percept.difference(percept)
            socket.send(repr(difference))

        j += 1
        if (j == 3):
            print "Time to quit."
            quit = True

    for socket in clientsockets:
        socket.close()

