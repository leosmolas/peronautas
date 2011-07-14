# -*- coding: utf-8 -*-

# TODO:
# - fix behaviour in case the server dies unexpectedly on the agent, make sure he cleans up and dies gracefully.
# - i would bet my life that this comment was made by Iñakor... just because of............this ^

import sys
import time
import argparse
from timeit                         import Timer
from connection.MASSimConnection    import MASSimConnection
from connection.MessageHandling     import *
from perceptServer.PerceptServer    import PerceptConnection, VortexPerceptConnection
from pyswip.prolog                  import Prolog
from pyswip.easy                    import *

####################################################################################################
class VortexWriter():
    # Clase auxiliar utilizada en caso de que NO se escriba en logs.
    def write(self, s):
        pass

    def close(self):
        pass



####################################################################################################
class Agent():
    
    def __init__(self, USER, PASS, useLog, perceptServerHost, perceptServerPort):
        self.username = USER
        self.password = PASS
        self.useLog   = useLog
        self.massimConnection = MASSimConnection('127.0.0.1', 12300, USER, PASS)
        if useLog:
            self.log   = open(USER + '.txt', 'w')
            sys.stdout = self.log
        else:
            self.log   = sys.stdout
            pass

        self.log.write("Basic initialization ")
        if (perceptServerPort and perceptServerHost):
            self.perceptConnection = PerceptConnection(perceptServerHost, int(perceptServerPort))
        else:
            self.perceptConnection = VortexPerceptConnection()
        self.log.write("done\n")



    def connect(self):
        # Connect and authenticate.
        self.massimConnection.connect()
        self.perceptConnection.connect()



    def disconnect(self):
        self.log.close()
        self.massimConnection.disconnect()
        self.perceptConnection.disconnect()



    def processActionRequest(self, action_id, msg_dict_private, msg_dict_public):
        self.log.write("@Agent: received request-action. id: %s\n" % action_id)
        action_xml = action(action_id, "skip")
        return action_xml



    def processSimulationStart(self, msg_dict):
        pass



    def processBye(self, msg_dict):
        pass



    def perceiveActLoop(self):
        # Receive simulation start notification.
        self.log.write("@Agent: waiting for simulation start notification.\n")
        xml = self.massimConnection.receive()
        _, _, msg_dict, _ = parse_as_dict(xml)

        self.log.write("@Agent: received simulation start notification.\n")
        print_message(msg_dict)
        #self.log.write("\nXML:\n")
        #self.log.write(xml)
        #self.log.write("\n")
        self.processSimulationStart(msg_dict)

        quit = False
        step = 0
        while (not quit):
            step += 1
            self.log.write("@Agent: step: %s" % step)

            xml = self.massimConnection.receive()
            msg_type, action_id, msg_dict_private, msg_dict_public = parse_as_dict(xml)

            #self.log.write("\nXML:\n")
            #self.log.write(xml)
            #self.log.write("\n")

            time.sleep(0.5)
            if (msg_type == 'request-action'):
                action_xml = self.processActionRequest(action_id, msg_dict_private, msg_dict_public)
                self.massimConnection.send(action_xml)
            elif (msg_type == 'sim-end'):
                self.log.write("@Agent: received sim-end\n")
                print_message(msg_dict_private)
            elif (msg_type == 'bye'):
                self.log.write("@Agent: received bye\n")
                print_message(msg_dict_private)
                self.processBye(msg_dict_private)
                quit = True
            else:
                self.log.write("@Agent: en area 51\n")
                print_message(msg_dict_private)
                quit = True



####################################################################################################
class PrologAgent(Agent):

    def __init__(self, USER, PASS, log, perceptServerHost, perceptServerPort, prolog_source):
        Agent.__init__(self, USER, PASS, log, perceptServerHost, perceptServerPort)
        self.log.write("Prolog initialization ")
        # Creo una conexion con SWI.
        self.prolog = Prolog()
        self.prolog.consult(prolog_source)
        if self.useLog:
            self.prolog.query("open('" + self.username + "-kb.txt', write, S), set_output(S)").next()
            #open('kb.txt', write, Stream),
            #set_output(Stream),
            #listing,
            #close(Stream)
        self.log.write("done\n")



    def disconnect(self):
        Agent.disconnect(self)
        print "PROLOG DATABASE DUMP:"
        self.prolog.query("listing(knode)").next()
        self.prolog.query("listing(kedge)").next()
        self.prolog.query("listing(kposition)").next()
        self.prolog.query("current_output(S), close(S)").next()



    def processSimulationStart(self, msg_dict):
        role = msg_dict['role']
        if   (role == 'Explorer'):
            self.prolog_role_file = 'pl/explorer.pl'
        elif (role == 'Repairer'):
            seld.prolog_role_file = 'pl/repairer.pl'
        elif (role == 'Sentinel'):
            self.prolog_role_file = 'pl/sentinel.pl'
        elif (role == 'Saboteur'):
            self.prolog_role_file = 'pl/saboteur.pl'
        elif (role == 'Inspector'):
            self.prolog_role_file = 'pl/inspector.pl'
        else:
            self.log.write("    @PrologAgent: error: unknown role\n")
        self.prolog.consult(self.prolog_role_file)

        # Guardo mi nombre en la KB.
        self.prolog.query("updateMyName(%s)" % self.username).next()



    def processNodes(self, msg_dict):
        # Obtenemos todos los vertices sondeados.
        # Despues, para cada vertice visible, nos fijamos si
        # el vertice esta entre los vertices sondeados
        # Si lo esta, se actualiza la informacion del verice con su valor, 
        # sino, se actualiza con el valor unknown.
        probed_verts = msg_dict.get('probed_verts', [])
        for x in msg_dict.get('vis_verts', []):
            # Esta el vertice entre los vertices sondeados?
            in_pv = False
            for pv in probed_verts:
                if (pv['name'] == x['name']):
                    in_pv = True
                    break
            if (in_pv):
                #print "El nodo %s esta entre los nodos sondeados" % x['name']
                self.prolog.query('updateNode(knode(%s,%s,%s))' % (x['name'], pv['value'], x['team'])).next()
            else:
                #print "El nodo %s no esta entre los nodos sondeados" % x['name']
                self.prolog.query('updateNode(knode(%s,unknown,%s))' % (x['name'], x['team'])).next()



    def processEdges(self, msg_dict):
        # Actualizamos el estado del mapa con los arcos.
        for e in msg_dict.get('vis_edges', []):
            self.prolog.query("updateEdge(kedge(%s,%s,unknown))" % (e['node1'], e['node2'])).next()

        for e in msg_dict.get('surveyed_edges', []):
            self.prolog.query("updateEdge(kedge(%s,%s,%s))" % (e['node1'], e['node2'], e['weight'])).next()



    def processEntities(self, msg_dict):
        self.prolog.query("retractall(kposition(_, _))").next()
        self.prolog.query("retractall(hposition(_, _))").next()
        for e in msg_dict.get('vis_ents'):
            if (e['name'] == ''):
                self.prolog.query("updateEntityPosition(unknown,%s)" % (e['node'])).next()
                self.prolog.query("updateEntityTeam(unknown,%s)" % (e['team'])).next()
            else:
                self.prolog.query("updateEntityPosition(%s,%s)" % (e['name'], e['node'])).next()
                self.prolog.query("updateEntityTeam(%s,%s)" % (e['name'], e['team'])).next()



    def processPerception(self, msg_dict_private, msg_dict_public):
        self.processEntities(msg_dict_public)
        self.processNodes(msg_dict_public)
        self.processEdges(msg_dict_public)
        
        # Actualizamos cada uno de los campos individuales del agente.
        self.prolog.query("updateEnergy(%s)"             % msg_dict_private['energy']).next()
        self.prolog.query("updateLastAction(%s)"        % msg_dict_private['last_action']).next()
        self.prolog.query("updateLastActionResult(%s)" % msg_dict_private['last_action_result']).next()
        self.prolog.query("updateMoney(%s)"              % msg_dict_private['money']).next()
        self.prolog.query("updateMaxHealth(%s)"         % msg_dict_private['max_health']).next()
        self.prolog.query("updateMaxEnergy(%s)"         % msg_dict_private['max_energy']).next()
        self.prolog.query("updatePosition(%s)"           % msg_dict_public['position']).next()



    def processActionRequest(self, action_id, msg_dict_private, msg_dict_public):
        self.log.write("    @PrologAgent: received request-action. id: %s\n" % action_id)

        # Synchronize perceptions with others.
        self.perceptConnection.send(msg_dict_public)
        percept_difference = self.perceptConnection.recv()

        self.log.write("\n    @PrologAgent: PERCEPTION: \n")
        print_message(msg_dict_public)
        self.log.write("\n")

        # Process perception.
        self.processPerception(msg_dict_private, msg_dict_public)
        query_result = self.prolog.query("exec(X)").next()
        actionList   = query_result['X']
        if   len(actionList) == 1:
            action_xml = action(action_id, actionList[0])
            self.log.write("    @PrologAgent: sending %s\n" % actionList[0])
        elif len(actionList) == 2:
            self.log.write("    @PrologAgent: sending %s %s\n" % (actionList[0], actionList[1]))
            action_xml = action(action_id, actionList[0], actionList[1])
        else:
            self.log.write("    @PrologAgent: error in returned action.\n")
            self.log.write("    @PrologAgent: return value: %s\n" % actionList)
            action_xml = action(action_id, "skip")
        return action_xml



####################################################################################################
if (__name__== "__main__"):
    parser = argparse.ArgumentParser(description="Pyeismassim agent initializer")
    parser.add_argument('user',     metavar='USER',                      help="the agent's username")
    parser.add_argument('password', metavar='PASSWORD',                  help="the agent's password")
    parser.add_argument('-sh',      metavar='SH_PERCEPTION_SERVER_HOST', help="use shared perception server on specified host",                                   dest='perceptServerHost')
    parser.add_argument('-sp',      metavar='SH_PERCEPTION_SERVER_PORT', help="use shared perception server on specified port",                                   dest='perceptServerPort')
    parser.add_argument('-l',                                            help="write-to-log mode.",                             action='store_const', const=True, dest='log')
    args = parser.parse_args()
    user, password, log, perceptServerHost, perceptServerPort = args.user, args.password, args.log, args.perceptServerHost, args.perceptServerPort

    agent = PrologAgent(user, password, log, perceptServerHost, perceptServerPort, "pl/kb.pl")
    agent.connect()
    agent.perceiveActLoop()
    agent.disconnect()

