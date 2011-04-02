import socket
import sys

class serverConnection:
    def __init__(self):
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        print "@serverConnection: socket initialized"

    def connect(self, host, port):
        self.lastHost = host
        self.lastPort = port
        print "@serverConnection: Asking for connection with %s and %s"% (host,port)
        self.sock.connect((host, port))

    def send(self, msg):
        self.sock.send(msg)

    def receive(self):
        msg = ''
        tmp = self.sock.recv(1024)
        if(not tmp):
            #si no se recibio nada, se perdio la conexion!)
            print "@serverConnection: DESCONNECTION DETECTED, retrying..."
            self.connect(self.lastHost, self.lastPort)
            tmp = self.receive()
        # por ahora asumo que llega un unico mensaje de longitud fija,
        # para evitar que la momia me devore
##        else:
#            print "Entering WHILE"
#            while tmp:
#                print "Entered while"
#                msg += tmp
#                tmp = self.sock.recv(1024)
#            msg += tmp
        print "@serverConnection: message received! '%s'" % msg
        return msg

    def disconnect(self):
        self.sock.close()
