# -*- coding: utf-8 -*-
import sys
import time
from timeit import Timer
from connection.MASSimConnection import MASSimConnection
from connection.MessageHandling import * 
from pyswip.prolog import Prolog
from pyswip.easy import *

HOST = "127.0.0.1"
PORT = 12300
USER = "a1"
PASS = "1"
assert_ = Functor("assert")
assert_ = Functor("assert")



def processPerception(msg,p):
    # p.query("last_action(_)") # %(x,x,msg[x]))
    # p.assertz("last_action(skip)")
    for x in ['position','energy','last_action','last_action_result','money']: 
        print x,msg[x],list(p.query("retract(%s(_))" % x))
        print bool(list(p.query("assert(%s(%s))" % (x,msg[x]))))
    aux = []
    for x in msg['vis_verts']:
        
        aux.append(x['name'])
    vert = "["
    for x in aux:
        vert+=x + ","
    vert2 = vert[:-1]+"]"
    
    print vert2,list(p.query("actualizarListas(%s,verts)" % vert2 ))
    aux = []
    for x in msg['vis_edges']:
        print (x['node1'],x['node2'])
        aux.append((x['node1'],x['node2']))
    vert = "["
    print "aux",aux
    for x in aux:
        vert+="arco(%s,%s)," %(x[0],x[1])
    vert2 = vert[:-1]+"]"
    print vert2,list(p.query("actualizarListas(%s,edges)" % vert2 ))
    

if __name__=="__main__":
    log = open('xml.txt', 'w')
    # Connect and authenticate.
    connection = MASSimConnection()
    connection.connect(HOST, PORT, USER, PASS)

    # Receive simulation start notification.
    print "@Agent: Waiting for simulation start notification."
    xml = connection.receive()
    print xml
    log.write(xml)
    msg = parse(xml)

    print "@Agent: received:"
    print_message(msg)

    steps = int(msg['steps'])

    # creo una conexion con SWI
    prolog = Prolog()
    prolog.consult("pl/kb.pl")

    for step in range(1,11):
        print "@Agent: step", step
        xml = connection.receive()
        log.write(xml)
        msg = parse(xml)	
        
        action_id = msg['id']
        print "Action id:", action_id
        
        #######
        # Process perception
        #######
        
        processPerception(msg,prolog)
        print prolog.query("verts(X)").next()["X"]
        print prolog.query("edges(X)").next()["X"]
        #le consulto a prolog que accion tomar
        # accion = list(prolog.query("action(X)"))
        

        # connection.send(action(action_id, accion.next()["X"])) #le pongo next porque puede devolver varias rtas (como si pusieras ;)
        connection.send(action(action_id, "skip")) #le pongo next porque puede devolver varias rtas (como si pusieras ;)

    log.close()

    # Send bye message and disconnect.
    connection.send(bye())
    connection.disconnect()
