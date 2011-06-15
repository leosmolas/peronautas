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

    def process_perception(self, dict_msg, p):
        # Actualiza la posicion, energia, ultima accion, resultado de la ultima accion, el dinero, la salud maxima y la enegria maxima.
        for x in ['position', 'energy', 'last_action', 'last_action_result', 'money', 'max_health', 'max_energy']: 
            p.query("retract(%s(_))" % x).next()
            p.query("assert(%s(%s))" % (x, dict_msg[x])).next()

        # Actualiza la lista de vertices, almacenados en el predicado dinamico verts/1.
        aux = []
        for x in dict_msg['vis_verts']:
            aux.append(x['name'])
        vert = "["
        for x in aux:
            vert += x + ","
        vert2 = vert[:-1] + "]"
        p.query("actualizarListas(%s, verts)" % vert2 ).next()

        # Actualiza la lista de arcos, almacenada en el predicado dinamico edges/1.
        aux = []
        for x in dict_msg['vis_edges']:
            aux.append((x['node1'], x['node2']))
        vert = "["
        for x in aux:
            vert += "edge(%s,%s)," % (x[0], x[1])
        vert2 = vert[:-1] + "]"
        p.query("actualizarListas(%s,edges)" % vert2 ).next()
        
    def perceive_act_loop(self, prolog_source):

        # Receive simulation start notification.
        print "@Agent: Waiting for simulation start notification."
        xml = self.connection.receive()
        #self.log.write(xml)
        _, _, dict_msg = parse_as_dict(xml)

        print "@Agent: received:"
        print_dict_message(dict_msg)

        steps = int(dict_msg['steps'])

        # Creo una conexion con SWI.
        prolog = Prolog()
        prolog.consult(prolog_source)

        quit = False
        step = 0
        while (not quit):
            step += 1
            print "Step:", step
            # Receive action request.
            xml = self.connection.receive()
            msg_type, action_id, dict_msg = parse_as_dict(xml)
            _,        _,         list_msg = parse_as_list(xml)

            # DEGUG: imprimir la percepcion parseada a prolog
            #print_list_message(list_msg)
            #self.log.write("PROLOG PERCEPT:\n")
            #for line in list_msg:
            #    self.log.write('   ' + line + '\n')

            time.sleep(1.5)
            if (msg_type == 'request-action'):
                print "@Agent: received request-action. id:", action_id

                # Process perception.
                self.process_perception(dict_msg, prolog)
                t1 = prolog.query("verts(X)").next()["X"]
                t2 = prolog.query("edges(X)").next()["X"]
                print "t1:", t1, "t2:", t2
                #prolog.query("searchNeigh(X)").next()["X"]
                prolog.query("argumentation").next()
                prolog.query("planning").next()
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
