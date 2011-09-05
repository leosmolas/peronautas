import time
import signal
import socket
import sys
import cProfile
import cPickle
import base64

BUFSIZE = 4096

# [x] uso de shutdown
# [x] get rid of base64 encoding
# [ ] mechear chequeo de tiempo en agent.py, reduciendo el tiempo que queda, pasandole el cutoff a prolog
# [ ] timeout del receive del servidor
#       si se cae, que el PS no incluya la percepcion del agente caido en los demas
#       mensaje por defecto para asumir para los demas?
#       se bardean los demas si no saben algo de otro agente?
#       que se le envia al agente que llego tarde?
#       que se hace con el mensaje que llegara del agente ? descartarlo?
# [ ] sockets no bloqueantes

####################################################################################################
def recv_data(sckt):

    # Protocol Version 3
    stop = False
    buf = ''
    msg = ''
    while (not stop):
        buf = sckt.recv(BUFSIZE)
        if (len(buf) > 0):
            msg += buf
        else:
            stop = True

    # Deserialize
    # REPR
    #lst = eval(msg)

    # PICKLE
    #data = base64.b64decode(msg)
    lst  = cPickle.loads(msg)
    return lst

####################################################################################################
def send_data(sckt, data):
    # Serialize
    # REPR
    #string = repr(data)

    # PICKLE
    msg        = cPickle.dumps(data, cPickle.HIGHEST_PROTOCOL)
    #safe_data  = base64.b64encode(msg)
    bytes_sent = sckt.send(msg)
    return bytes_sent

####################################################################################################
class VortexPerceptConnection():

    def __init__(self):
        pass

    def disconnect(self):
        pass

    def send_and_recv(self, data):
        return dict([])

####################################################################################################
class PerceptConnection():
    """
    This is the client's connection to the shared percept server.
    """

    def __init__(self, host, port, name):
        self.host    = host
        self.port    = port
        self.name    = name

    def disconnect(self):
        self.sckt = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.sckt.connect((self.host, self.port))
        msg = frozenset([ (-1, ), (0, self.name) ])
        send_data(self.sckt, msg)
        self.sckt.close()

    def send_and_recv(self, dictionary):

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

        # Send
        print "@PerceptConnection: Connecting to %s:%s" % (self.host, self.port)
        self.sckt = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.sckt.connect((self.host, self.port))
        print "@PerceptConnection: succesfully connected."
        lst  = dict2list(self.name, dictionary)
        fset = frozenset(lst)
        send_data(self.sckt, fset)
        self.sckt.shutdown(1)
        print "@PerceptConnection: sent data."

        # Receive
        lst = recv_data(self.sckt)
        self.sckt.close()
        return list2dict(lst)

####################################################################################################

def signal_handler(signal, frame):
    serverSocket.close()
    print ""
    print "Socket closed successfully"
    print "Percept server shutdown."
    sys.exit(0)

class LogFile():

    def __init__(self, path, mode):
        self.fileobj = open(path, mode)

    def write(self, string):
        print string
        self.fileobj.write(string)
        self.fileobj.write("\n")

    def close(self):
        self.fileobj.close()

    def writeStatistics(self, agentlist, turn, reception, send, total):
        agentstring = ''
        agentdict = {}
        for agent in agentlist:
            agentdict[agent.name] = ((agent.rE - agent.rS), (agent.sE - agent.sS), (agent.sE - agent.rS), agent.bS, agent.bR)
        for key in sorted(agentdict.keys()):
            agentdata = agentdict[key]
            agentstring += "%s,%s,%s,%s,%s," % agentdata
        # format is: turn, reception time, send time, total time, agent1 reception time, agent1 send time, agent1 total time, agent1 bytes sent, agent1 bytes received, agent2 ...
        self.fileobj.write("%s,%s,%s,%s,%s\n" % (turn, reception, send, total, agentstring))

class AgentConnection():

    def __init__(self):
        self.name      = ''
        self.connected = False
        self.sckt      = None
        self.percept   = None
        self.rS        = 0 # reception start
        self.rE        = 0 # reception end
        self.sS        = 0 # send start
        self.sE        = 0 # send end
        self.bS        = 0 # bytes sent
        self.bR        = 0 # bytes received
    
####################################################################################################
if (__name__ == "__main__"):

    if (len(sys.argv) == 4):
        CONNECTIONS = int(sys.argv[1])
        HOST        =     sys.argv[2]
        PORT        = int(sys.argv[3])
        logFile        = LogFile('log.txt',   'w')
        statisticsFile = LogFile('stats.csv', 'w')
    else:
        print ""
        print "         Usage: python PerceptServer.py NUMBER_CONNECTIONS HOST_IP PORT_NUMBER"
        print "                                 Port 10000 is recommended."
        print ""
        print "                 Example: python PerceptServer.py 10 192.168.0.10 10000"
        print ""
        sys.exit()

    signal.signal(signal.SIGINT, signal_handler)

    ADDR    = (HOST, PORT)
    agents  = [AgentConnection() for i in range(CONNECTIONS)]

    serverSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    serverSocket.bind((ADDR))
    serverSocket.listen(CONNECTIONS)

    logFile.write( "" )
    logFile.write( "Percept server started." )
    logFile.write( "Listening on: %s : %s" % (HOST, PORT) )
    logFile.write( "The Computer Name is: %s" % socket.gethostname() )
    logFile.write( "" )

    #for i in range(CONNECTIONS):
    #    sckt, address = serverSocket.accept()
    #    agents[i].connected = True
    #    agents[i].sckt = sckt
    #    agents[i].name = agents[i].sckt.recv(BUFSIZE)
    #    print          "Server: received a connection from %s at address %s" % (address, agents[i].name)
    #    logFile.write( "Server: received a connection from %s at address %s" % (address, agents[i].name) )
    
    turn = 0
    quit = False
    while (not quit):
        logFile.write( "Iteration: %s" % turn )

        # Initialize the global percept to an empty frozenset.
        globalPercept = frozenset([])

        #----------------------------------------------------------------------#
        logFile.write( "    Reception phase." )
        receptionStart = time.time()

        for i in range(CONNECTIONS):

            sckt, address = serverSocket.accept()
            agents[i].connected = True
            agents[i].sckt = sckt
            agents[i].rS = time.time()
            
            # Receive perception.
            agents[i].percept = recv_data(agents[i].sckt)

            # Get the agent's name.
            for x in agents[i].percept:
                if (x[0] == 0):
                    agents[i].name = x[1]
                    break
            # Check whether it was a goodbye message.
            if ((-1, ) in agents[i].percept):
                logFile.write( "        Agent %s: sent goodbye message." % (agents[i].name) )
                agents[i].sckt.shutdown(socket.SHUT_RDWR)
                agents[i].sckt.close()
                agents[i].connected = False
            else:
                globalPercept = globalPercept.union(agents[i].percept)

            agents[i].rE = time.time()
            agents[i].bS = len(agents[i].percept)
            logFile.write( "        Server: received a connection from %s at address %s" % (agents[i].name, address) )
            logFile.write( "        Agent %s received %s bytes" % (agents[i].name, agents[i].bS) )
            logFile.write( "        Agent %s reception time: %s" % (agents[i].name, agents[i].rE - agents[i].rS) )
                
        receptionEnd = time.time()
        logFile.write( "        Total reception time: %s" % (receptionEnd - receptionStart) )
        #----------------------------------------------------------------------#

        #----------------------------------------------------------------------#
        logFile.write( "    Sending phase." )
        sendingStart = time.time()

        for i in range(CONNECTIONS):
            # For each agent, calculate the difference between the global percept accumulator
            # and the original percept and send the difference.
            agents[i].sS = time.time()

            if (agents[i].connected):
                difference = globalPercept.difference(agents[i].percept)
                agents[i].bR = send_data(agents[i].sckt, difference)
                agents[i].sckt.shutdown(socket.SHUT_RDWR)
                agents[i].sckt.close()
                logFile.write( "        Agent %s: sending %s bytes." % (agents[i].name, agents[i].bR) )

            agents[i].sE = time.time()
            logFile.write( "        Agent %s sending time: %s" % (agents[i].name, agents[i].sE - agents[i].sS))

        sendingEnd = time.time()
        logFile.write( "        Sending time: %s" % (sendingEnd - sendingStart) )
        #----------------------------------------------------------------------#

        logFile.write( "Total turn time: %s" % (sendingEnd - receptionStart) )
        statisticsFile.writeStatistics(agents, turn, (receptionEnd - receptionStart), (sendingEnd - sendingStart), (sendingEnd - receptionStart))

        turn += 1
        # If at least one agent is connected, don't quit.
        connectedAgents = [agent.name for agent in agents if agent.connected]
        print "Connected agents:", connectedAgents
        if (len(connectedAgents) == 0):
            logFile.write( "Time to quit." )
            quit = True
            
    serverSocket.close()
    logFile.close()
    statisticsFile.close()

