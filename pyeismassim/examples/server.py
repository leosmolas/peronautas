import sys
import socket
import threading
import SocketServer
from SocketServer import TCPServer
from SocketServer import BaseRequestHandler
from SocketServer import ThreadingMixIn
from time import strftime

class ThreadedTCPRequestHandler(BaseRequestHandler):

    def handle(self):
        # self.request is the socket object
        print "@tcpserver: %s request from ip=%s port=%s" % (
            strftime("%Y-%m-%d %H:%M:%S"),
            self.client_address[0],
            self.client_address[1]
            )
        data = self.request.recv(1024)
        cur_thread = threading.currentThread()
        response = "%s: %s" % (cur_thread.getName(), data)
        self.request.send(response)
        
class ThreadedTCPServer(SocketServer.ThreadingMixIn, SocketServer.TCPServer):
    def __init__(self):
        self.daemon_threads = False

if __name__ == "__main__":
    HOST    = "127.0.01"  #we are the host
    PORT    = 1099        #arbitrary port not currently in use
    ADDR    = (HOST,PORT) #we need a tuple for the address
    BUFSIZE = 4096        #reasonably sized buffer for data

    server  = TCPServer(ADDR, ThreadedTCPRequestHandler)

    server_thread = threading.Thread(target = server.serve_forever)
    server_thread.setDaemon(True)
    server_thread.start()
    print "Server loop running in thread: ", server_thread.getName()
    
    print "Press enter to shutdown server."
    sys.stdin.readline()
