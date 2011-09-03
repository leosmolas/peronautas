import time
import signal
import socket
import sys
import cProfile
import cPickle

# - si se cae, que se pueda reconectar
#   el server tendra identificar quien es quien
#   agregar fase de autenticacion
# - si se cae, que el PS se de cuenta, el socket, para poder renovarlo
# - si se cae, que el PS no incluya la percepcion del agente caido en los demas

####################################################################################################
class VortexPerceptConnection():

    def connect(self, name):
        pass

    def disconnect(self):
        pass

    def send(self, user, data):
        pass

    def recv(self):
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

    def connect(self, name):
        print "@PerceptConnection: Connecting to %s:%s" % (self.host, self.port)
        self.socket.connect((self.host, self.port))
        self.socket.send(name + '\0')
        print "@PerceptConnection: succesfully connected."

    def disconnect(self):
        self.socket.send('EOF\0')
        self.socket.close()

    def send(self, username, dictionary):

        def dict2list(username, dictionary):
            position_list = dictionary.get('position')
            position      = position_list[0]
            node          = position.get('node')
            vis_range     = position.get('vis_range')
            if 'role' in position:
                result = [ ( 0
                           , username
                           , node
                           , vis_range
                           , position['health']
                           , position['max_health']
                           , position['role']
                           ) ]
            else:
                result = [ ( 0
                           , username
                           , node
                           , vis_range
                           , position['health']
                           , position['max_health']
                           ) ]
            for v in dictionary.get('vis_verts', []):
                result.append( ( 1 
                               , v['name']
                               , v['team']
                               ) )
            for v in dictionary.get('vis_edges', []):
                result.append( ( 2
                               , v['node1']
                               , v['node2']
                               ) )
            for v in dictionary.get('vis_ents',  []):
                result.append( ( 3
                               , v['node']
                               , v['name']
                               , v['team']
                               , v['status']
                               ) )
            for v in dictionary.get('probed_verts', []):
                result.append( ( 4
                               , v['name']
                               , v['value']
                               ) )
            for v in dictionary.get('surveyed_edges', []):
                result.append( ( 5
                               , v['node1']
                               , v['node2']
                               , v['weight']
                               ) )
            for v in dictionary.get('inspected_ents', []):
                result.append( ( 6
                               , v['name']
                               , v['team']
                               , v['node']
                               , v['role']
                               , v['energy']
                               , v['max_energy']
                               , v['health']
                               , v['max_health']
                               , v['strength']
                               , v['vis_range']
                               ) )
            return result

        lst    = dict2list(username, dictionary)
        fset   = frozenset(lst)
        string = repr(fset)
        self.socket.send(string)
        self.socket.send('\0')

    def recv(self):

        def list2dict(stringlist):
            result = { 'position'       : []
                     , 'vis_verts'      : []
                     , 'vis_edges'      : []
                     , 'vis_ents'       : []
                     , 'probed_verts'   : []
                     , 'surveyed_edges' : []
                     , 'inspected_ents' : []
                     }
            for p in stringlist:
                if   (p[0] == 0):
                    if len(p) == 6:
                        result['position'].append(   { 'name'       : p[1]
                                                     , 'node'       : p[2]
                                                     , 'vis_range'  : p[3]
                                                     , 'health'     : p[4]
                                                     , 'max_health' : p[5]
                                                     } )
                    else:
                        result['position'].append(   { 'name'       : p[1]
                                                     , 'node'       : p[2]
                                                     , 'vis_range'  : p[3]
                                                     , 'health'     : p[4]
                                                     , 'max_health' : p[5]
                                                     , 'role'       : p[6]
                                                     } )
                elif (p[0] == 1):
                    result['vis_verts'].append(      { 'name' : p[1]
                                                     , 'team' : p[2]
                                                     } )
                elif (p[0] == 2):
                    result['vis_edges'].append(      { 'node1' : p[1]
                                                     , 'node2' : p[2]
                                                     } )
                elif (p[0] == 3):
                    result['vis_ents'].append(       { 'node'  : p[1]
                                                     , 'name'  : p[2]
                                                     , 'team'  : p[3]
                                                     , 'status': p[4]
                                                     } )
                elif (p[0] == 4):
                    result['probed_verts'].append(   { 'name'  : p[1]
                                                     , 'value' : p[2]
                                                     } )
                elif (p[0] == 5):
                    result['surveyed_edges'].append( { 'node1'  : p[1]
                                                     , 'node2'  : p[2]
                                                     , 'weight' : p[3]
                                                     } )
                elif (p[0] == 6):
                    result['inspected_ents'].append( { 'name'       : p[1]
                                                     , 'team'       : p[2]
                                                     , 'node'       : p[3]
                                                     , 'role'       : p[4]
                                                     , 'energy'     : p[5]
                                                     , 'max_energy' : p[6]
                                                     , 'health'     : p[7]
                                                     , 'max_health' : p[8]
                                                     , 'strength'   : p[9]
                                                     , 'vis_range'  : p[10]
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

def signal_handler(signal, frame):
        print ''
        print 'You pressed Ctrl+C!'
        serverSocket.close()
        print 'Socket closed successfully'
        print "Percept server shutdown."
        sys.exit(0)

class AgentConnection():

    def __init__(self):
        self.name      = ""
        self.connected = False
        self.socket    = None
        self.percept   = None
        self.t0        = 0
        self.t1        = 0

####################################################################################################
if (__name__ == "__main__"):

    if (len(sys.argv) == 4):
        CONNECTIONS = int(sys.argv[1])
        HOST        =     sys.argv[2]
        PORT        = int(sys.argv[3])
    else:
        print ""
        print ""
        print "         Usage: python PerceptServer.py NUMBER_CONNECTIONS HOST_IP PORT_NUMBER"
        print "                                 Port 10000 is recommended."
        print ""
        print "                 Example: python PerceptServer.py 10 192.168.0.10 10000"
        print ""
        sys.exit()

    ADDR = (HOST, PORT)
    BUFSIZE = 4096

    agents = [AgentConnection() for x in range(CONNECTIONS)]

    # Accept CONNECTION client sockets.
    serverSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    serverSocket.bind((ADDR))
    serverSocket.listen(CONNECTIONS)

    signal.signal(signal.SIGINT, signal_handler)

    print ""
    print "Percept server started."
    print "Listening on: %s : %s" % (HOST, PORT)
    print "The Computer Name is: %s" % socket.gethostname()
    print ""

    for i in range(CONNECTIONS):
        socket, address = serverSocket.accept()
        agents[i].connected = True
        agents[i].socket = socket
        agents[i].name = agents[i].socket.recv(BUFSIZE)
        print "Server: received a connection from %s at address %s" % (address, agents[i].name)
    
    j = 0
    quit = False
    while (not quit):
        print "Iteration: %s" % j
        # Initialize the global percept to an empty frozenset.
        globalPercept = frozenset([])

        print "    Reception phase."
        receptionStart = time.time()
        for i in range(CONNECTIONS):
            if (agents[i].connected):
                agents[i].t0 = time.time()
                stop = False
                msg  = ''
                while (not stop):
                    msg += agents[i].socket.recv(BUFSIZE)
                    if (len(msg) > 0):
                        stop = (msg[-1] == '\0')
                    else:
                        print "        Agent %s: The message received was empty!" % (agents[i].name)
                        stop = True
                percept = msg[:-1]
                
                if (percept[:3] == 'EOF'):
                    print "        Agent %s: Client sent EOF, closing its socket." % (agents[i].name)
                    agents[i].socket.close()
                    agents[i].connected = False
                else:
                    agents[i].percept = eval(percept)
                    globalPercept = globalPercept.union(agents[i].percept)
                agents[i].t1 = time.time()
                print "        Agent %s: time: %s" % (agents[i].name, agents[i].t1 - agents[i].t0)
        receptionEnd = time.time()
        print "        Reception time: %s" % (receptionEnd - receptionStart)

        print "    Sending phase."
        sendingStart = time.time()
        for i in range(CONNECTIONS):
            # For each socket, calculate the difference between the accumulator
            # and the original percept and send the difference.
            if (agents[i].connected):
                difference = globalPercept.difference(agents[i].percept)
                # print "CONNECTION:", i
                # print "DIFFERENCE:", difference
                agents[i].socket.send(repr(difference))
                agents[i].socket.send('\0')
        sendingEnd = time.time()
        print "        Sending time: %s" % (sendingEnd - sendingStart)

        print "Total turn time: %s" % (sendingEnd - receptionStart)

        j += 1
        # If at least one agent is connected, don't quit.
        connectedAgents = len([0 for agent in agents if agent.connected])
        if (connectedAgents == 0):
            print "Time to quit."
            quit = True
            
    serverSocket.close()

