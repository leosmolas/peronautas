import socket

class Connection:
    def __init__(self):
        pass

if (__name__ == "__main__"):

    CONNECTIONS = 10
    HOST = socket.gethostname()
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
       clientsockets.append(clientsocket)
    
    while true
        # initialize the global percept to an empty frozenset
        global_percept = frozenset([])

        for socket in clientsockets:
            percept = socket.receive(BUFSIZE)
            percept_list = eval(percept)
            percept_set = frozenset(percept_list)
            global_percept = global_percept.union(percept_set)

        for socket in clientsockets:

    #   for each socket
    #       calculate the difference between the accumulator and the original percept
    #       send the difference

    for socket in clientsockets:
        socket.close()
