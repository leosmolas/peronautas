import sys
import socket
from serverConnection import serverConnection

def client(ip, port, message):
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.connect((ip, port))
    sock.send(message)
    response = sock.recv(1024)
    print "Received: %s" % response
    sock.close()

if __name__ == "__main__":
    HOST    = "127.0.01"  #we are the host
    PORT    = 1099        #arbitrary port not currently in use
    ADDR    = (HOST,PORT) #we need a tuple for the address
    BUFSIZE = 4096        #reasonably sized buffer for data
    
    serverConn = serverConnection()
    serverConn.connect("127.0.0.1", 1099) #por algun motivo, se usa el puerto que carajo quiere
    while True:
        print "@Script: Enter message: ",
        msg = sys.stdin.readline()
        client(HOST, PORT, str(msg))
        #serverConn.send(msg)
        #msg = serverConn.receive()
        #print "@Script: Received: '%s'" % firstmsg
        #print "@Script: message sent"

    serverConn.close()
    print "@Script: Succesfully disconnected from server!"

