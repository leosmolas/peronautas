import socket
import sys
import time

MAX_CONNECTION_TRIES = 10

class MASSimConnection:

    def __init__(self):
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        print "@Connection: socket initialized."


    def connect(self, host, port, username, password):
        self.host     = host
        self.port     = port
        self.username = username
        self.password = password
        print "@Connection: connecting to " + host + ":" + str(port)
        code = self.sock.connect_ex((host, port))
        if (code == 0):
            self.connected = True
            print "@Connection: succeeded. socket name:", self.sock.getsockname()
        else:
            self.connected = False
            print "@Connection: failed. Error:", code
        time.sleep(2)
        self.authenticate(username, password)

    def disconnect(self):
        self.sock.shutdown(socket.SHUT_RDWR)
        self.sock.close()
        self.connected = False

    def send(self, msg):
        """
        Calcula la longitud del mensaje a enviar, y mantiene la cantidad de 
        bytes enviados en cada llamada a send(), y sigue llamando hasta que se 
        hayan enviado todos los bytes. El string debe terminar en un byte nulo
        para que el server sepa que termino la transmision. 
        Lo hacemos aca adentro para desligar al programador usuario de esa 
        responsabilidad.
        """
        if (self.connectionValid()):
            msg = msg + "\0"
            msg_length = len(msg)
            bytes_sent = 0
            while (bytes_sent < msg_length):
                sent = self.sock.send(msg[bytes_sent:])
                bytes_sent += sent
                print "@Connection: sent %s bytes: %s" %(sent, msg[:bytes_sent])
                if (sent == 0):
                    self.connected = False
                    raise RuntimeError("Server connection lost!")
        else:
            raise("Server connection lost!")


    def receive(self):
        """
        Recibe del socket de a bloques de 256 bytes. Mientras no se detecte el 
        final del mensaje, que deberia ser </message>, intenta recibir mas 
        informacion. Devuelve el string recibido por el socket.
        """
        if (self.connectionValid()):
            print "@Connection: starting to receive a message..."
            stop = False
            msg  = ''
            while (not stop):
                msg += self.sock.recv(2048)
                if (msg[-1]=='\0'):
                    stop = True
            print "@Connection: message received from socket: %s" % msg
            return msg
        else:
            raise("Server connection lost")


    def connectionValid(self):
        i = 0
        while (not self.connected) and (i < MAX_CONNECTION_TRIES):
            self.connect(self.host, self.port, self.username, self.password)
            i += 1
            #si no logramos conectarnos, hacemos espera progresiva
            sleep(i)
        return self.connected
            

    def authenticate(self, user, password):
        if (self.connectionValid()):
            sxmlh = u'<?xml version="1.0" encoding="UTF-8" standalone="no"?>'
            authentication_message = sxmlh + u'<message type="auth-request"><authentication password="' + password + '" username="' + user + '"/></message>'
            print "@Connection: sending authentication message."
            self.send(authentication_message)
            print "@Connection: waiting for reply."
            authentication_reply = self.receive()
        else:
            raise("Server connection lost")

            # TODO: validate reply message, and return True or False according 
            # result.
