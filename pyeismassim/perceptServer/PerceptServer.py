import socket
import sys

####################################################################################################
class VortexPerceptConnection():

    def connect(self):
        pass

    def disconnect(self):
        pass

    def send(self, data):
        pass

    def recv(self):
        return {}

####################################################################################################
class PerceptConnection():
    """
    This is the client's connection to the shared percept server.
    """

    def __init__(self, host, port):
        self.host    = host
        self.port    = port
        self.bufsize = 4096
        self.socket  = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

    def connect(self):
        self.socket.connect((self.host, self.port))

    def disconnect(self):
        self.socket.send('EOF')
        self.socket.close()

    def send(self, dictionary):

        def dict2list(dictionary):
            result = []
            position  = dictionary.get('position',  [])
            vis_verts = dictionary.get('vis_verts', [])
            vis_edges = dictionary.get('vis_edges', [])
            vis_ents  = dictionary.get('vis_ents',  [])

            result += ("position", position)
            for v in vis_verts:
                result += ('vis_vert', v['name'],  v['team'] )
            for v in vis_edges:
                result += ('vis_edge', v['node1'], v['node2'] )
            for v in vis_ents:
                result += ('vis_ent',  v['node'],  v['name'], v['team'] )
            
            return result

        lst    = dict2list(dictionary)
        #lst    = dictionary.items()
        fset   = frozenset(lst)
        string = repr(fset)
        self.socket.send(string)

    def recv(self):

        def list2dict(stringlist):
            result = { 'vis_verts' : [], 'vis_edges' : [], 'vis_ents' :[] }
            for p in stringlist:
                if (p[0] == 'position'):
                    result['position'] = p[1]
                elif(p[0] == 'vis_vert'):
                    result['vis_verts'] += { 'name'  : p[1], 'team'  : p[2] }
                elif (p[0] == 'vis_edge'):
                    result['vis_edges'] += { 'node1' : p[1], 'node2' : p[2] }
                elif (p[0] == 'vis_ent'):
                    result['vis_ents']  += { 'node'  : p[1], 'name'  : p[2], 'team' : p[3]}
                else:
                    pass
            return result

        string = self.socket.recv(self.bufsize)
        lst    = eval(string)
        return list2dict(lst)

####################################################################################################
if (__name__ == "__main__"):

    if (len(sys.argv) == 3):
        CONNECTIONS = int(sys.argv[1])
        PORT        = int(sys.argv[2])
    else:
        print "Usage: python PerceptServer.py CONNECTIONS PORT"
        print "Port 10000 is recommended."
        sys.exit()

    HOST = 'localhost'
    ADDR = (HOST, PORT)
    BUFSIZE = 4096

    clientSocket          = [None  for x in range(CONNECTIONS)]
    clientSocketConnected = [False for x in range(CONNECTIONS)]

    # Accept CONNECTION client sockets.
    serverSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    serverSocket.bind((ADDR))
    serverSocket.listen(CONNECTIONS)

    print "Percept server started."

    for i in range(CONNECTIONS):
        socket, address = serverSocket.accept()
        print "Server: received a connection. address:", address
        clientSocket[i] = socket
        clientSocketConnected[i] = True
    
    j = 0
    quit = False
    while (not quit):
        print "Iteration:", j
        # Initialize the global percept to an empty frozenset.
        globalPercept = frozenset([])
        percepts      = range(CONNECTIONS)

        for i in range(CONNECTIONS):
            percept  = clientSocket[i].recv(BUFSIZE)
            if (percept == 'EOF'):
                print "Client sent EOF, closing its socket."
                clientSocket[i].close()
                clientSocketConnected[i] = False
            else:
                #print "PERCEPT:", percept
                percepts[i]   = eval(percept)
                globalPercept = globalPercept.union(percepts[i])

        for i in range(CONNECTIONS):
            # for each socket, calculate the difference between the accumulator
            # and the original percept and send the difference.
            if (clientSocketConnected[i]):
                percept    = percepts[i]
                difference = globalPercept.difference(percept)
                clientSocket[i].send(repr(difference))

        j += 1
        connectedClients = len([0 for y in clientSocketConnected if y])
        if (connectedClients == 0):
            print "Time to quit."
            quit = True
    serverSocket.close()

