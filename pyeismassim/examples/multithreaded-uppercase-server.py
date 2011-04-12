import SocketServer
import netstring
import sys
import string

class MyRequestHandler(SocketServer.BaseRequestHandler):
    def handle(self):
        print "Connected:", self.client_address
        while 1:
            rq = netstring.readns(self.request)
            print "From:", self.client_address, rq
            sys.stdout.flush()
            if rq == "":
                break
            netstring.writens(self.request, string.upper(rq))

myServer = SocketServer.ThreadingTCPServer(('', 8081), MyRequestHandler)
myServer.serve_forever()

