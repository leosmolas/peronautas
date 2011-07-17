import socket
import sys

####################################################################################################
class VortexPerceptConnection():

    def connect(self):
        pass

    def disconnect(self):
        pass

    def send(self, user, data):
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

    def send(self, username, dictionary):

        def dict2list(username, dictionary):
            result = [    (0, 
                            username, 
                            dictionary.get('position', 'unknown')
                          )
                     ]
            for v in dictionary.get('vis_verts', []):
                result.append((1, 
                            v['name'],
                            v['team']
                          ))
            for v in dictionary.get('vis_edges', []):
                result.append((2, 
                            v['node1'],
                            v['node2']
                          ))
            for v in dictionary.get('vis_ents',  []):
                result.append((3,
                            v['node'],
                            v['name'],
                            v['team']
                          ))
            for v in dictionary.get('probed_verts', []):
                result.append((4, 
                            v['name'], 
                            v['value']
                          ))
            for v in dictionary.get('surveyed_edges', []):
                result.append((5, 
                            v['node1'], 
                            v['node2'], 
                            v['weight']
                          ))
            for v in dictionary.get('inspected_ents', []):
                result.append((6, 
                            v['energy'], 
                            v['health'], 
                            v['max_energy'], 
                            v['max_health'], 
                            v['name'], 
                            v['node'], 
                            v['role'], 
                            v['strength'], 
                            v['team'], 
                            v['vis_range']
                          ))
            return result

        lst    = dict2list(username, dictionary)
        fset   = frozenset(lst)
        string = repr(fset)
        self.socket.send(string)

    def recv(self):

        def list2dict(stringlist):
            result = { 'position'       : [],
                       'vis_verts'      : [],
                       'vis_edges'      : [],
                       'vis_ents'       : [],
                       'probed_verts'   : [],
                       'surveyed_edges' : [],
                       'inspected_ents' : [] 
                       }
            for p in stringlist:
                if   (p[0] == 0):
                    result['position']       += { 'agent' : p[1],
                                                  'node'  : p[2] 
                                                }
                elif (p[0] == 1):
                    result['vis_verts']      += { 'name' : p[1],
                                                  'team' : p[2] 
                                                }
                elif (p[0] == 2):
                    result['vis_edges']      += { 'node1' : p[1],
                                                  'node2' : p[2] 
                                                }
                elif (p[0] == 3):
                    result['vis_ents']       += { 'node' : p[1],
                                                  'name' : p[2],
                                                  'team' : p[3] 
                                                }
                elif (p[0] == 4):
                    result['probed_verts']   += { 'name'  : p[1],
                                                  'value' : p[2] 
                                                }
                elif (p[0] == 5):
                    result['surveyed_edges'] += { 'node1'  : p[1],
                                                  'node2'  : p[2],
                                                  'weight' : p[3] 
                                                }
                elif (p[0] == 6):
                    result['inspected_ents'] += { 'energy'     : p[1],
                                                  'health'     : p[2],
                                                  'max_energy' : p[3],
                                                  'max_health' : p[4],
                                                  'name'       : p[5],
                                                  'node'       : p[6],
                                                  'role'       : p[7],
                                                  'strength'   : p[8],
                                                  'team'       : p[9],
                                                  'vis_range'  : p[10]
                                                }
                else:
                    print "@PerceptServerConnection: decode error: ", p
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

        print "Reception phase."
        for i in range(CONNECTIONS):
            percept  = clientSocket[i].recv(BUFSIZE)
            if (percept == 'EOF'):
                print "Client sent EOF, closing its socket."
                clientSocket[i].close()
                clientSocketConnected[i] = False
            else:
                #print "CONNECTION:", i
                #print "PERCEPT:", percept
                percepts[i]   = eval(percept)
                globalPercept = globalPercept.union(percepts[i])

        print "Sending phase."
        for i in range(CONNECTIONS):
            # for each socket, calculate the difference between the accumulator
            # and the original percept and send the difference.
            if (clientSocketConnected[i]):
                percept    = percepts[i]
                difference = globalPercept.difference(percept)
                print "CONNECTION:", i
                print "DIFFERENCE:", difference
                clientSocket[i].send(repr(difference))

        j += 1
        connectedClients = len([0 for y in clientSocketConnected if y])
        if (connectedClients == 0):
            print "Time to quit."
            quit = True
    serverSocket.close()

