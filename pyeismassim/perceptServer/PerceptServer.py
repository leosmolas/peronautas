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
        #return { 'fake' : 'dict' }
        return dict([])

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
        self.socket.send('EOF\0')
        self.socket.close()

    def send(self, username, dictionary):

        def dict2list(username, dictionary):
            position_list = dictionary.get('position')
            position      = position_list[0]
            node          = position.get('node')
            result = [    (0,
                            username,
                            node
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
                            v['name'], 
                            v['team'], 
                            v['node'], 
                            v['role'], 
                            v['energy'], 
                            v['max_energy'], 
                            v['health'], 
                            v['max_health'], 
                            v['strength'], 
                            v['vis_range']
                          ))
            return result

        lst    = dict2list(username, dictionary)
        fset   = frozenset(lst)
        string = repr(fset)
        self.socket.send(string)
        self.socket.send('\0')

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
                    result['position'].append(      { 'name' : p[1],
                                                      'node' : p[2] 
                                                    })
                elif (p[0] == 1):
                    result['vis_verts'].append(     { 'name' : p[1],
                                                      'team' : p[2] 
                                                    })
                elif (p[0] == 2):
                    result['vis_edges'].append(     { 'node1' : p[1],
                                                      'node2' : p[2] 
                                                    })
                elif (p[0] == 3):
                    result['vis_ents'].append(      { 'node' : p[1],
                                                      'name' : p[2],
                                                      'team' : p[3] 
                                                    })
                elif (p[0] == 4):
                    result['probed_verts'].append(  { 'name'  : p[1],
                                                      'value' : p[2] 
                                                    })
                elif (p[0] == 5):
                    result['surveyed_edges'].append({ 'node1'  : p[1],
                                                      'node2'  : p[2],
                                                      'weight' : p[3] 
                                                    })
                elif (p[0] == 6):
                    result['inspected_ents'].append({ 'name'       : p[1],
                                                      'team'       : p[2],
                                                      'node'       : p[3],
                                                      'role'       : p[4],
                                                      'energy'     : p[5],
                                                      'max_energy' : p[6],
                                                      'health'     : p[7],
                                                      'max_health' : p[8],
                                                      'strength'   : p[9],
                                                      'vis_range'  : p[10]
                                                    })
                else:
                    print "@PerceptServerConnection: decode error: ", p
            return result

        stop = False
        msg  = ''
        while (not stop):
            msg += self.socket.recv(self.bufsize)
            if (len(msg) > 0):
                stop = (msg[-1] == '\0')
            else:
                print "@PerceptConnection: the message received was empty!"
                stop = True

        lst = eval(msg[:-1])
        return list2dict(lst)

####################################################################################################
if (__name__ == "__main__"):

    if (len(sys.argv) == 4):
        CONNECTIONS = int(sys.argv[1])
        HOST        =     sys.argv[2]
        PORT        = int(sys.argv[3])
    else:
        print "Usage: python PerceptServer.py CONNECTIONS HOST PORT"
        print "Port 10000 is recommended."
        sys.exit()

    ADDR = (HOST, PORT)
    BUFSIZE = 4096

    clientSocket          = [None  for x in range(CONNECTIONS)]
    clientSocketConnected = [False for x in range(CONNECTIONS)]

    # Accept CONNECTION client sockets.
    serverSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    serverSocket.bind((ADDR))
    serverSocket.listen(CONNECTIONS)

    print "Percept server started."
    print "Listening on: %s : %s" % (HOST, PORT) 

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

            stop = False
            msg  = ''
            while (not stop):
                msg += clientSocket[i].recv(BUFSIZE)
                if (len(msg) > 0):
                    stop = (msg[-1] == '\0')
                else:
                    print "The message received was empty!"
                    stop = True
            percept = msg[:-1]
            
            if (percept[:3] == 'EOF'):
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
                clientSocket[i].send('\0')

        j += 1
        connectedClients = len([0 for y in clientSocketConnected if y])
        if (connectedClients == 0):
            print "Time to quit."
            quit = True
    serverSocket.close()

