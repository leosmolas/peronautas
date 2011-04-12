import socket
import netstring

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect(('127.0.0.1', 8081))
while 1:
    arg = raw_input("Send: ")
    if not arg:
        netstring.writens(s, "end")
        break
    netstring.writens(s, arg)
    ret = netstring.readns(s)
    print "Returned:", ret
s.close()

