import socket
import sys
import time
from BeautifulSoup import BeautifulStoneSoup

class MASSimConnection:

    def __init__(self):
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        # Standard XML Header
        self.sxmlh = u'<?xml version="1.0" encoding="UTF-8" standalone="no"?>'
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
            print "@Connection: failed. error:", code
        time.sleep(2)
        self.authenticate(username, password)

    def disconnect(self):
        self.sock.shutdown(socket.SHUT_RDWR)
        self.sock.close()
        self.connected = False

    def send(self, msg):
        # TODO: 
        # - make sure the termination check is ok, verify the server also 
        # null-terminates strings and we can use the same mechanism to ensure
        # complete data transmission.
        """
        Calcula la longitud del mensaje a enviar, y mantiene la cantidad de 
        bytes enviados en cada llamada a send(), y sigue llamando hasta que se 
        hayan enviado todos los bytes. El string debe terminar en un byte nulo
        para que el server sepa que termino la transmision. 
        Lo hacemos aca adentro para desligar al programador usuario de esa 
        responsabilidad.
        """
        if (self.connected):
            msg = msg + "\0"
            msg_length = len(msg)
            print "@Connection: message length:", msg_length
            bytes_sent = 0
            while (bytes_sent < msg_length):
                sent = self.sock.send(msg[bytes_sent:])
                bytes_sent += sent
                print "@Connection: sent", sent, "bytes:", msg[:bytes_sent]
                if (sent == 0):
                    raise RuntimeError("socket connection broken")
            print "@Connection: exiting send."

    def receive(self):
        """
        Recibe del socket de a bloques de 256 bytes. Mientras no se detecte el 
        final del mensaje, que deberia ser </message>, intenta recibir mas 
        informacion. Devuelve el string recibido por el socket.
        """
        if (self.connected):
            stop = False
            msg  = ''
            while (not stop):
                msg += self.sock.recv(256)
                if (msg[-10:] == "</message>"):
                    stop = True
            print "@Connection: message received:", msg
            return msg

    def authenticate(self, username, password):
        if (self.connected):
            authentication_message = self.sxmlh + u'<message type="auth-request"><authentication password="' + password + '" username="' + username + '"/></message>'
            print "@Connection: sending authentication message."
            self.send(authentication_message)
            print "@Connection: waiting for reply."
            authentication_reply = self.receive()
            print "@Connection: received: ", authentication_reply

            xml = BeautifulStoneSoup(authentication_reply)
            # TODO: validate reply message, and return True or False according 
            # to result.
