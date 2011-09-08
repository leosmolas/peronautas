import time
import signal
import socket
import sys
import cProfile
import cPickle
import base64

# ver que va a quedando en la kb turno a turno y que se puede sacar
# [ ] timeout del receive del servidor
#       si se cae, que el PS no incluya la percepcion del agente caido en los demas
#       mensaje por defecto para asumir para los demas?
#       se bardean los demas si no saben algo de otro agente?
#       que se le envia al agente que llego tarde?
#       que se hace con el mensaje que llegara del agente ? descartarlo?
# [ ] threads

# - probar reconectarnos y llegar tarde
# - que el percept server sincronize el comienzo?
# LIMPIAR AGENT.PY
#     make the agent send a connect messsage to the percept server
# TESTEAR LA RECONEXION
# TESTEAR LA HISTORY PERCEPT
# TESTEAR TIMEOUT
# TESTEAR TIMEOUT PERPCET SERVER RECEIVE DEL LADO DEL AGENTE
BUFSIZE = 4096

####################################################################################################
def recv_data(sckt):
    stop = False
    buf = ''
    msg = ''
    while (not stop):
        buf = sckt.recv(BUFSIZE)
        if (len(buf) > 0):
            msg += buf
        else:
            stop = True
    bytes_received = len(msg)
    return bytes_received, cPickle.loads(msg)

####################################################################################################
def send_data(sckt, data):
    # Serialize
    msg        = cPickle.dumps(data, cPickle.HIGHEST_PROTOCOL)
    bytes_sent = sckt.send(msg)
    return bytes_sent

####################################################################################################
class VortexPerceptConnection():

    def __init__(self):
        pass

    def connect(self):
        pass

    def disconnect(self):
        pass

    def send_and_recv(self, data, reconnect = False):
        return dict([])

####################################################################################################
class PerceptConnection():
    """
    This is the client's connection to the shared percept server.
    """

    def __init__(self, host, port, name):
        self.host = host
        self.port = port
        self.name = name
        self.sckt = None

    def disconnect(self):
        self.sckt = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.sckt.connect((self.host, self.port))
        msg = { 'message_type' : 'goodbye'
              , 'name'         : self.name
              }
        send_data(self.sckt, msg)
        self.sckt.close()

    def connect(self):
        self.sckt = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.sckt.connect((self.host, self.port))
        msg = { 'message_type' : 'reconnect'
              , 'name'         : self.name
              }
        send_data(self.sckt, msg)
        self.sckt.close()

    def send_and_recv(self, dictionary, reconnect = False):
        dictionary['name']         = self.name
        if (reconnect):
            dictionary['message_type'] = 'percept'
        else:
            dictionary['message_type'] = 'reconnect'

        self.sckt = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        try:
            # Send
            print "@PerceptConnection: Connecting to %s:%s" % (self.host, self.port)
            self.sckt.settimeout(1)
            self.sckt.connect((self.host, self.port))
            print "@PerceptConnection: succesfully connected,", 
            send_data(self.sckt, dictionary)
            self.sckt.shutdown(socket.SHUT_WR)
            print "sent data,",

            # Receive
            stop = False
            buf = ''
            msg = ''
            while (not stop):
                buf = self.sckt.recv(BUFSIZE)
                if (len(buf) > 0):
                    msg += buf
                else:
                    stop = True
            percept = cPickle.loads(msg)
            print "and received data."
        except:
            print "@PerceptServerConnection: receive from percept server timed out!"
            percept = dictionary
        self.sckt.close()
        return percept

####################################################################################################
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

####################################################################################################
class AgentConnection():

    def __init__(self):
        self.name      = ''
        self.connected = False
        self.sckt      = None
        self.percept   = None
        self.msg_type  = ''
        self.rS        = 0 # reception start
        self.rE        = 0 # reception end
        self.sS        = 0 # send start
        self.sE        = 0 # send end
        self.bS        = 0 # bytes sent
        self.bR        = 0 # bytes received
    
####################################################################################################
def dict2fset(dictionary):
    result = [ (0, dictionary['name'] ) ]
    position_list = dictionary.get('position')
    if (position_list):
        position  = position_list[0]
        node      = position.get('node')
        vis_range = position.get('vis_range')
        if ('role' in position):
            t = ( 1
                , dictionary['name']
                , node
                , vis_range
                , position['health']
                , position['max_health']
                , position['role']
                )
            result.append(t)
        else:
            t = ( 1
                , dictionary['name']
                , node
                , vis_range
                , position['health']
                , position['max_health']
                )
            result.append(t)
    for v in dictionary.get('vis_verts', []):
        t = ( 2 
            , v['name']
            , v['team']
            )
        result.append(t)
        historyPercept.add(t)
    for v in dictionary.get('vis_edges', []):
        t = ( 3
            , v['node1']
            , v['node2']
            )
        result.append(t)
        historyPercept.add(t)
    for v in dictionary.get('vis_ents',  []):
        t = ( 4
            , v['node']
            , v['name']
            , v['team']
            , v['status']
            )
        result.append(t)
    for v in dictionary.get('probed_verts', []):
        t = ( 5
            , v['name']
            , v['value']
            )
        historyPercept.add(t)
        result.append(t)
    for v in dictionary.get('surveyed_edges', []):
        t = ( 6
            , v['node1']
            , v['node2']
            , v['weight']
            )
        historyPercept.add(t)
        result.append(t)
    for v in dictionary.get('inspected_ents', []):
        t = ( 7
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
            )
        result.append(t)
    return frozenset(result)

def fset2dict(fset):
    result = { 'position'       : []
             , 'vis_verts'      : []
             , 'vis_edges'      : []
             , 'vis_ents'       : []
             , 'probed_verts'   : []
             , 'surveyed_edges' : []
             , 'inspected_ents' : []
             }
    for p in fset:
        if   (p[0] == 0):
            pass
        elif (p[0] == 1):
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
        elif (p[0] == 2):
            result['vis_verts'].append(      { 'name' : p[1]
                                             , 'team' : p[2]
                                             } )
        elif (p[0] == 3):
            result['vis_edges'].append(      { 'node1' : p[1]
                                             , 'node2' : p[2]
                                             } )
        elif (p[0] == 4):
            result['vis_ents'].append(       { 'node'  : p[1]
                                             , 'name'  : p[2]
                                             , 'team'  : p[3]
                                             , 'status': p[4]
                                             } )
        elif (p[0] == 5):
            result['probed_verts'].append(   { 'name'  : p[1]
                                             , 'value' : p[2]
                                             } )
        elif (p[0] == 6):
            result['surveyed_edges'].append( { 'node1'  : p[1]
                                             , 'node2'  : p[2]
                                             , 'weight' : p[3]
                                             } )
        elif (p[0] == 7):
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

def signal_handler(signal, frame):
    serverSocket.close()

    dumpfile = open('percept_history', 'wb')
    cPickle.dump(historyPercept, dumpfile, cPickle.HIGHEST_PROTOCOL)
    dumpfile.close()
    print "Socket closed successfully"
    print "Percept server shutdown."
    sys.exit(0)

if (__name__ == "__main__"):

    historyPercept = set([])
    argc = len(sys.argv)
    if ((argc == 4) or (argc == 5)):
        CONNECTIONS = int(sys.argv[1])
        HOST        =     sys.argv[2]
        PORT        = int(sys.argv[3])
        logFile        = LogFile('logs/log.txt',   'w')
        statisticsFile = LogFile('logs/stats.csv', 'w')
        if (argc == 5):
            print "Loading percept history."
            loadfile = open('percept_history', 'rb')
            historyPercept = cPickle.load(loadfile)
        else:
            dumpfile = open('percept_history', 'wb')
            dumpfile.close()
            
    else:
        print ""
        print "         Usage: python PerceptServer.py NUMBER_CONNECTIONS HOST_IP PORT_NUMBER [-l]"
        print "                                 Port 10000 is recommended."
        print ""
        print "                 Example: python PerceptServer.py 10 192.168.0.10 10000"
        print "                 Example: python PerceptServer.py 10 192.168.0.10 10000 -l"
        print ""
        sys.exit(1)

    signal.signal(signal.SIGINT, signal_handler)

    ADDR    = (HOST, PORT)
    agents  = [AgentConnection() for i in range(CONNECTIONS)]

    serverSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    serverSocket.bind((ADDR))
    serverSocket.listen(CONNECTIONS)

    logFile.write( "                        " )
    logFile.write( "Percept server started. " )
    logFile.write( "Listening on: %s : %s   " % (HOST, PORT) )
    logFile.write( "The Computer Name is: %s" % socket.gethostname() )
    logFile.write( "                        " )

    turn = 0
    quit = False
    while (not quit):
        logFile.write( "Iteration: %s" % turn )

        # Initialize the global percept to an empty frozenset.
        globalPercept  = frozenset([])

        #----------------------------------------------------------------------#
        logFile.write( "    Reception phase." )
        receptionStart = time.time()

        for i in range(CONNECTIONS):
            
            # Receive perception.
            agents[i].sckt, address    = serverSocket.accept()
            agents[i].rS               = time.time()
            agents[i].connected        = True
            agents[i].bS, percept_dict = recv_data(agents[i].sckt)
            agents[i].name             = percept_dict['name']
            agents[i].msg_type         = percept_dict['message_type']
            agents[i].percept          = dict2fset(percept_dict)
            logFile.write( "        Server: received a connection from %s at address %s" % (agents[i].name, address) )

            # Check the message.
            if (agents[i].msg_type == 'goodbye'):
                logFile.write( "          Agent %s: received goodbye message." % (agents[i].name) )
                agents[i].sckt.shutdown(socket.SHUT_RDWR)
                agents[i].sckt.close()
                agents[i].connected = False
            else:
                # The message type is either 'percept' or 'reconnect'
                globalPercept = globalPercept.union(agents[i].percept)
                agents[i].rE = time.time()
                logFile.write( "          Agent %s: received %s bytes" % (agents[i].name, agents[i].bS) )
                logFile.write( "          Agent %s: reception time: %s" % (agents[i].name, agents[i].rE - agents[i].rS) )
                
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
                if (agents[i].msg_type == 'reconnect'):
                    # The agent has reconnected.
                    difference.union(historyPercept)
                data = fset2dict(difference)
                print "TYPE:",type(data)
                agents[i].bR = send_data(agents[i].sckt, data)
                #agents[i].sckt.shutdown(socket.SHUT_RDWR)
                agents[i].sckt.close()
                logFile.write( "        Agent %s: sending %s bytes." % (agents[i].name, agents[i].bR) )

            agents[i].sE = time.time()
            logFile.write( "        Agent %s: sending time: %s" % (agents[i].name, agents[i].sE - agents[i].sS))

        sendingEnd = time.time()
        logFile.write( "        Total sending time: %s" % (sendingEnd - sendingStart) )
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

