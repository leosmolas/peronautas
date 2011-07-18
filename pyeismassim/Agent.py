# -*- coding: utf-8 -*-

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
class Agent():
    
    def __init__(self, USER, PASS, useLog, perceptServerHost, perceptServerPort):
        self.username = USER
        self.password = PASS
        self.useLog   = useLog
        self.massimConnection = MASSimConnection('127.0.0.1', 12300, USER, PASS)
        if useLog:
            sys.stdout = open(USER + '-log.txt', 'w')
        else:
            pass
        self.log = sys.stdout

        print "Basic initialization ",
        if (perceptServerPort and perceptServerHost):
            self.perceptConnection = PerceptConnection(perceptServerHost, int(perceptServerPort))
        else:
            self.perceptConnection = VortexPerceptConnection()
        print "done"



    def connect(self):
        # Connect and authenticate.
        self.massimConnection.connect()
        self.perceptConnection.connect()



    def disconnect(self):
        self.log.close()
        self.log = sys.__stdout__
        sys.stdout = sys.__stdout__
        self.massimConnection.disconnect()
        self.perceptConnection.disconnect()



    def processActionRequest(self, action_id, msg_dict_private, msg_dict_public):
        print "@Agent: received request-action. id: %s" % action_id
        action_xml = action(action_id, "skip")
        return action_xml



    def processSimulationStart(self, msg_dict):
        pass



    def processBye(self, msg_dict):
        pass



    def perceiveActLoop(self):
        # Receive simulation start notification.
        print "@Agent: waiting for simulation start notification."
        xml = self.massimConnection.receive()
        _, _, msg_dict, _ = parse_as_dict(xml)

        print "@Agent: received simulation start notification."
        print_message(msg_dict)
        #print "\nXML:"
        #print xml
        #print ""
        self.processSimulationStart(msg_dict)

        quit = False
        step = 0
        while (not quit):
            step += 1
            print "@Agent: step: %s" % step

            xml = self.massimConnection.receive()
            msg_type, action_id, msg_dict_private, msg_dict_public = parse_as_dict(xml)

            #print "\nXML:"
            #print xml
            #print ""

            time.sleep(0.5)
            if (msg_type == 'request-action'):
                action_xml = self.processActionRequest(action_id, msg_dict_private, msg_dict_public)
                self.massimConnection.send(action_xml)
            elif (msg_type == 'sim-end'):
                print "@Agent: received sim-end"
                print_message(msg_dict_private)
            elif (msg_type == 'bye'):
                print "@Agent: received bye"
                print_message(msg_dict_private)
                self.processBye(msg_dict_private)
                quit = True
            else:
                print "@Agent: en area 51"
                print_message(msg_dict_private)
                quit = True



####################################################################################################
class PrologAgent(Agent):

    def __init__(self, USER, PASS, log, perceptServerHost, perceptServerPort, prolog_source):
        Agent.__init__(self, USER, PASS, log, perceptServerHost, perceptServerPort)
        print "Prolog initialization",
        # Creo una conexion con SWI.
        self.prolog = Prolog()
        self.prolog.consult(prolog_source)
        if (log):
            self.prolog.query("redirect_output('" + self.username + "-kb.txt')").next()
        print "done"



    def disconnect(self):
        self.prolog.query("dumpKB").next()
        self.prolog.query("close_output").next()
        Agent.disconnect(self)



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
            print "    @PrologAgent: error: unknown role"
        self.prolog.consult(self.prolog_role_file)

        # Guardo mi nombre en la KB.
        self.prolog.query("updateMyName(%s)" % self.username).next()



    def merge_percepts(self, msg_dict_public, msg_dict_difference):
        msg_dict_public['position'].extend(       msg_dict_difference.get('position',       []))
        msg_dict_public['vis_verts'].extend(      msg_dict_difference.get('vis_verts',      []))
        msg_dict_public['vis_edges'].extend(      msg_dict_difference.get('vis_edges',      []))
        msg_dict_public['vis_ents'].extend(       msg_dict_difference.get('vis_ents',       []))
        msg_dict_public['probed_verts'].extend(   msg_dict_difference.get('probed_verts',   []))
        msg_dict_public['surveyed_edges'].extend( msg_dict_difference.get('surveyed_edges', []))
        msg_dict_public['inspected_ents'].extend( msg_dict_difference.get('inspected_ents', []))



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
                self.prolog.query('updateNodeValue(%s,%s)' % (x['name'], pv['value'])).next()
            else:
                #print "El nodo %s no esta entre los nodos sondeados" % x['name']
                self.prolog.query('updateNodeValue(%s,unknown)' % (x['name'])).next()
            self.prolog.query('updateNodeTeam(%s,%s)' % (x['name'], x['team'])).next()



    def processEdges(self, msg_dict):
        # Actualizamos el estado del mapa con los arcos.
        for e in msg_dict.get('vis_edges', []):
            self.prolog.query("updateEdge(%s,%s,unknown)" % (e['node1'], e['node2'])).next()

        for e in msg_dict.get('surveyed_edges', []):
            self.prolog.query("updateEdge(%s,%s,%s)" % (e['node1'], e['node2'], e['weight'])).next()



    def processEntities(self, msg_dict):
        for p in msg_dict.get('position', []):
            if (p['name'] == 'self'):
                p['name'] = self.username
            self.prolog.query("updatePosition(%s,%s)" % (p['name'], p['node'])).next()

        for e in msg_dict.get('vis_ents'):
            self.prolog.query("updateEntity(%s,%s,%s,unknown,unknown,unknown,unknown,unknown,unknown,unknown)" % (e['name'], e['team'], e['node'])).next()

        for e in msg_dict.get('inspected_ents', []):
            self.prolog.query("updateEntity(%s,%s,%s,%s,%s,%s,%s,%s,%s,%s)" % (e['name'], e['team'], e['node'], e['role'], e['energy'], e['max_energy'], e['health'], e['max_health'], e['strength'], e['vis_range'])).next()


    def processPerception(self, msg_dict_private, msg_dict_public):
        # Actualizamos cada uno de los campos individuales del agente.
        self.prolog.query("updateStep(%s)"              % msg_dict_private['step']).next()
        self.prolog.query("updateEnergy(%s)"            % msg_dict_private['energy']).next()
        self.prolog.query("updateMaxEnergy(%s)"         % msg_dict_private['max_energy']).next()
        self.prolog.query("updateMaxEnergyDisabled(%s)" % msg_dict_private['max_energy_disabled']).next()
        self.prolog.query("updateHealth(%s)"            % msg_dict_private['health']).next()
        self.prolog.query("updateMaxHealth(%s)"         % msg_dict_private['max_health']).next()
        self.prolog.query("updateStrength(%s)"          % msg_dict_private['strength']).next()
        self.prolog.query("updateVisualRange(%s)"       % msg_dict_private['vis_range']).next()
        self.prolog.query("updateLastAction(%s)"        % msg_dict_private['last_action']).next()
        self.prolog.query("updateLastActionResult(%s)"  % msg_dict_private['last_action_result']).next()
        self.prolog.query("updateMoney(%s)"             % msg_dict_private['money']).next()
        self.prolog.query("updateScore(%s)"             % msg_dict_private['score']).next()
        self.prolog.query("updateZoneScore(%s)"         % msg_dict_private['zone_score']).next()
        self.prolog.query("updateLastStepScore(%s)"     % msg_dict_private['last_step_score']).next()
        self.processNodes(msg_dict_public)
        self.processEdges(msg_dict_public)
        self.processEntities(msg_dict_public)



    def processActionRequest(self, action_id, msg_dict_private, msg_dict_public):
        print "    @PrologAgent: received request-action. id: %s" % action_id

        # Synchronize perceptions with others.
        self.perceptConnection.send(self.username, msg_dict_public)
        msg_dict_difference = self.perceptConnection.recv()
        self.merge_percepts(msg_dict_public, msg_dict_difference)

        #print "\n    @PrologAgent: PERCEPTION:"
        #print_message(msg_dict_public)
        #print ""

        # Process perception.
        self.processPerception(msg_dict_private, msg_dict_public)
        query_result = self.prolog.query("exec(X)").next()
        actionList   = query_result['X']
        if   len(actionList) == 1:
            action_xml = action(action_id, actionList[0])
            print "    @PrologAgent: sending %s" % actionList[0]
        elif len(actionList) == 2:
            print "    @PrologAgent: sending %s %s" % (actionList[0], actionList[1])
            action_xml = action(action_id, actionList[0], actionList[1])
        else:
            print "    @PrologAgent: error in returned action."
            print "    @PrologAgent: return value: %s" % actionList
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

    agent = PrologAgent(user, password, log, perceptServerHost, perceptServerPort, "pl/agent.pl")
    agent.connect()
    agent.perceiveActLoop()
    agent.disconnect()

