# -*- coding: utf-8 -*-

# TODO:
# - fix behaviour in case the server dies unexpectedly on the agent, make sure he cleans up and dies gracefully.

import sys
import time
from timeit import Timer
from connection.MASSimConnection import MASSimConnection
from connection.MessageHandling import *
from pyswip.prolog import Prolog
from pyswip.easy import *

class Agent():
    
    def __init__(self, USER, PASS):
        self.HOST = "127.0.0.1"
        self.PORT = 12300
        self.USER = USER
        self.PASS = PASS
        self.log = open('log-' + USER + '.txt', 'w')

    def connect(self):
        # Connect and authenticate.
        self.connection = MASSimConnection()
        self.connection.connect(self.HOST, self.PORT, self.USER, self.PASS)

    def disconnect(self):
        self.log.close()
        self.connection.disconnect()

    def perceive_act_loop(self):
        # Receive simulation start notification.
        print "@Agent: waiting for simulation start notification."
        xml = self.connection.receive()
        self.log.write(xml)
        _, _, msg = parse_as_dict(xml)

        print "@Agent: received simulation start notification."
        #print_dict_message(msg)

        quit = False
        step = 0
        while (not quit):
            step += 1
            print "@Agent: step:", step
            xml = self.connection.receive()
            self.log.write(xml)
            msg_type, action_id, msg = parse_as_list(xml)
            time.sleep(1.5)
            #reception = time.time()
            #while (time.time() - reception < 1.5):
            #    print "waiting"
            if (msg_type == 'request-action'):
                print "@Agent: received request-action. id:", action_id
                action_xml = action(action_id, "skip")
                self.log.write(action_xml)
                #print_list_message(msg) # DEBUG
                self.connection.send(action_xml)
            elif (msg_type == 'bye'):
                print "@Agent: received bye"
                quit = True
            elif (msg_type == 'sim-end'):
                print "@Agent: received sim-end"
                quit = True
            else:
                print "@Agent: en area 51"

        

class DummyAgent(Agent):

    def __init__(self):
        pass

    def perceive_act_loop(self, actions):
        pass

class PrologAgent(Agent):

    def processPerception(self, msg, prologConnection):
        # actualizamos cada uno de los campos individuales del agente
        for x in ['position', 'energy', 'last_action', 'last_action_result', 'money', 'max_health', 'max_energy']:
            # cambiamos retract por retractall porque hay problemas al principio
            prologConnection.query("retractall(%s(_))" % x).next() 
            prologConnection.query("assert(%s(%s))" % (x, msg[x])).next()
        print "@Agent: Mi posicion es %s" % msg['position']
        vert = "["
        # actualizamos el estado del mapa con los nodos
        for x in msg['vis_verts']:
            vert += "node(%s, unknown, %s)," % (x['name'], x['team'])
        vert2 = vert[:-1] + "]"
        print "@PrologAgent: Llamando!\nupdateNodes(%s)" % vert2
        
        prologConnection.query("updateNodes(%s)" % vert2 ).next()
        vert = "["
        # actualizamos el estado del mapa con los arcos
        for x in msg['vis_edges']:
            vert += "edge(%s,%s,unknown)," % (x['node1'],x['node2'])
        vert2 = vert[:-1]+"]"
        list(prologConnection.query("updateEdges(%s)" % vert2 ))
        
    def perceive_act_loop(self, prolog_source):

        # Receive simulation start notification.
        print "@Agent: Waiting for simulation start notification."
        xml = self.connection.receive()
        #self.log.write(xml)
        _, _, msg = parse_as_dict(xml)

        print "@Agent: received:"
        print_dict_message(msg)

        steps = int(msg['steps'])

        # Creo una conexion con SWI.
        prolog = Prolog()
        prolog.consult(prolog_source)

        quit = False
        step = 0
        while (not quit):
            step += 1
            print "Step:", step
            xml = self.connection.receive()
            #self.log.write(xml)
            msg_type, action_id, msg = parse_as_dict(xml)

            # DEGUG
            _, _, list_msg = parse_as_list(xml)
            print_list_message(list_msg)
            self.log.write("PROLOG PERCEPT:\n")
            for line in list_msg:
                self.log.write('   ' + line + '\n')

            time.sleep(1.5)
            if (msg_type == 'request-action'):
                print "@Agent: received request-action. id:", action_id

                # Process perception.
                self.processPerception(msg, prolog)
                list(prolog.query("argumentation"))
                list(prolog.query("planning"))
                actionList = prolog.query("exec(X)").next()["X"]
                if len(actionList) == 2:
                    action_xml = action(action_id, actionList[0], actionList[1])
                else:
                    action_xml = action(action_id, actionList[0])
                self.connection.send(action_xml)
                self.log.write(action_xml)
            elif (msg_type == 'bye'):
                print "@Agent: received bye"
                #quit = True
            elif (msg_type == 'sim-end'):
                print "@Agent: received sim-end"
                #quit = True
            else:
                quit = True
                print "@Agent: en area 51"

if (__name__== "__main__"):
    if (len(sys.argv) == 3):
        USER = sys.argv[1]
        PASS = sys.argv[2]
    else:
        print "Usage: python Agent.py USERNAME PASSWORD"
        sys.exit()

    agent = PrologAgent(USER, PASS)
    agent.connect()
    agent.perceive_act_loop("pl/kb.pl")
    agent.disconnect()

    #agent = Agent(USER, PASS)
    #agent.connect()
    #agent.perceive_act_loop()
    #agent.disconnect()

    #if (len(sys.argv) == 3):
    #    USER = sys.argv[1]
    #    PASS = sys.argv[2]
    #else:
    #    print "Usage: python Agent.py USERNAME PASSWORD"
    #    sys.exit()

    #agent = Agent(USER, PASS)
    #agent.connect()
    #agent.perceive_act_loop()
    #agent.disconnect()
