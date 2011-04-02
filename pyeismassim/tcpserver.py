from SocketServer import TCPServer
from SocketServer import BaseRequestHandler
from time import strftime

class MyHandler(BaseRequestHandler):
    def handle(self):
        # self.request is the socket object
        print "@tcpserver: %s I got a request from ip=%s port=%s" % (
            strftime("%Y-%m-%d %H:%M:%S"),
            self.client_address[0],
            self.client_address[1]
            )
        self.request.send("Hello! I'm the TCP Guru. What's your name?\n")
        print "@tcpserver: Welcomming message sent!"
        bufsize=1024
        response=self.request.recv(bufsize).strip() # or recv(bufsize, flags)
        data_to_send="Welcome %s!\n" % response
        self.request.send(data_to_send) # or send(data, flags)
        print "%s connection finnished" % self.client_address[0]
        
server = TCPServer(("127.0.0.1", 1029), MyHandler)
server.serve_forever()
