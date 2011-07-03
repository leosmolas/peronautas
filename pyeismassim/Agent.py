# -*- coding: utf-8 -*-

# TODO:
# - fix behaviour in case the server dies unexpectedly on the agent, make sure he cleans up and dies gracefully.
# - i would bet my life that this comment was made by Iñakor... just because of............this ^

import sys
import time
import argparse
from timeit import Timer
from connection.MASSimConnection import MASSimConnection
from connection.MessageHandling import *
from pyswip.prolog import Prolog
from pyswip.easy import *

####################################################################################################
class VortexWriter():#clase auxiliar utilizada en caso de que NO se escriba en logs
    def write(self, s):
        pass

    def close(self):
        pass
####################################################################################################
class Agent():
    
    def __init__(self, USER, PASS, useLog, sharedServerPort):
        print "Basic initialization"
        self.HOST = "127.0.0.1"
        self.PORT = 12300
        self.USER = USER
        self.PASS = PASS
        self.SHSERVERPORT = sharedServerPort
        if useLog:
            self.log  = open('log-' + USER + '.txt', 'w')
        else:
            self.log = VortexWriter()
        if sharedServerPort:
            #inicializar conexion con el server, supongo
            pass
        else:
            #imagino que deberemos inventar algo en caso de que NO se use, similar al VortexWriter
            pass

    def connect(self):
        # Connect and authenticate.
        self.connection = MASSimConnection()
        self.connection.connect(self.HOST, self.PORT, self.USER, self.PASS)       

    def disconnect(self):
        self.log.close()
        self.connection.disconnect()

    def processActionRequest(self, action_id, msg_dict_private, msg_dict_public):
        print "@Agent: received request-action. id:", action_id
        action_xml = action(action_id, "skip")
        return action_xml

    def perceiveActLoop(self):
        # Receive simulation start notification.
        print "@Agent: waiting for simulation start notification."
        xml = self.connection.receive()
        self.log.write(xml)
        _, _, msg_dict, _ = parse_as_dict(xml)

        print "@Agent: received simulation start notification."
        print_message_dict(msg_dict)

        quit = False
        steps = int(msg_dict['steps'])
        step = 0
        while (not quit):
            step += 1
            print "@Agent: step:", step

            # Receive action request.
            xml = self.connection.receive()
            msg_type, action_id, msg_dict_private, msg_dict_public = parse_as_dict(xml)

            self.log.write(xml)

            time.sleep(1.5)
            if (msg_type == 'request-action'):
                action_xml = self.processActionRequest(action_id, msg_dict_private, msg_dict_public)
                self.connection.send(action_xml)
                self.log.write(action_xml)
            elif (msg_type == 'bye'):
                print "@Agent: received bye"
                #quit = True
            elif (msg_type == 'sim-end'):
                print "@Agent: received sim-end"
                #quit = True
            else:
                print "@Agent: en area 51"
                quit = True
        
####################################################################################################
class PrologAgent(Agent):

    def __init__(self, USER, PASS, prolog_source, log, shServerPort):
        Agent.__init__(self, USER, PASS, log, shServerPort)
        print "Prolog initialization"
        # Creo una conexion con SWI.
        self.prolog = Prolog()
        self.prolog.consult(prolog_source)

    def processPerception(self, msg_dict_private, msg_dict_public):
        # Actualizamos cada uno de los campos individuales del agente.
        self.prolog.query("replace_energy(%s)"              % msg_dict_private['energy']).next()
        self.prolog.query("replace_last_action(%s)"         % msg_dict_private['last_action']).next()
        self.prolog.query("replace_last_action_result(%s)"  % msg_dict_private['last_action_result']).next()
        self.prolog.query("replace_money(%s)"               % msg_dict_private['money']).next()
        self.prolog.query("replace_max_health(%s)"          % msg_dict_private['max_health']).next()
        self.prolog.query("replace_max_energy(%s)"          % msg_dict_private['max_energy']).next()
        self.prolog.query("replace_position(%s)"            % msg_dict_public['position']).next()
        #print "@PrologAgent: Mi posicion es %s. Llamando!\nreplace_position(%s)" % (msg_dict_public['position'], msg_dict_public['position'])

        # Actualizamos el estado del mapa con los nodos.
        vert = "["
        for x in msg_dict_public['vis_verts']:
            vert += "node(%s, unknown, %s)," % (x['name'], x['team'])
        vert2 = vert[:-1] + "]"
        self.prolog.query("updateNodes(%s)" % vert2 ).next()
        #print "@PrologAgent %s: Llamando!\nupdateNodes(%s)" % (self.USER, vert2)
        
        # Actualizamos el estado del mapa con los arcos.
        vert = "["
        for x in msg_dict_public['vis_edges']:
            vert += "kedge(%s,%s,unknown)," % (x['node1'],x['node2'])
        vert2 = vert[:-1]+"]"
        self.prolog.query("updateEdges(%s)" % vert2 ).next()
        #print "@PrologAgent: Llamando!\nupdateEdges(%s)" % vert2

    def processActionRequest(self, action_id, msg_dict_private, msg_dict_public):
        print "@PrologAgent: received request-action. id:", action_id

        # Process perception.
        self.processPerception(msg_dict_private, msg_dict_public)
        list(self.prolog.query("argumentation"))
        list(self.prolog.query("planning"))
        actionList = self.prolog.query("exec(X)").next()['X']
        if len(actionList) == 2:
            action_xml = action(action_id, actionList[0], actionList[1])
        else:
            action_xml = action(action_id, actionList[0])
        return action_xml

####################################################################################################
if (__name__== "__main__"):
    parser = argparse.ArgumentParser(description="Pyeismassim agent initializer")
    parser.add_argument('user', metavar='USER', help="the agent's username")
    parser.add_argument('password', metavar='PASSWORD', help="the agent's password")
    parser.add_argument('-l', help="write-to-log mode.", action='store_const', const=True, dest='log')
    parser.add_argument('-s', metavar='SH_PERCEPTION_SERVER_PORT', help="use shared perception server on specified port", dest='shServerPort')

    args=parser.parse_args()

    user, password, log, shServerPort = args.user, args.password, args.log, args.shServerPort

    agent = PrologAgent(user, password, "pl/kb.pl", log, shServerPort)
    agent.connect()
    agent.perceiveActLoop()
    agent.disconnect()

