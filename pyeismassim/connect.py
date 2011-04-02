from serverConnection import serverConnection
# El siguiente codigo comentado resulta del primer experimento
# abriendo un socket a mano; es viejo, pero queda por una cuestion
# historica. Es como el Burrito Ortega, digamos (?).
#import socket
#sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
#sock.connect(("127.0.0.1", 1029))
#print sock.recv(1024)
#sock.send("First message")
#print "First message sent!"


# este codigo utiliza la clase serverConnection, para abstraerse
# (YEAH, RIGHT) del manejo de sockets.
serverConn = serverConnection()
serverConn.connect("127.0.0.1", 1029) #por algun motivo, se usa el puerto que carajo quiere
print "@Script: Waiting to receive a message"
firstmsg = serverConn.receive()
print "@Script: My a-first message was '%s'" % firstmsg
serverConn.send("First message")
print "@Script: First message sent"
secmsg = serverConn.receive()
print "@Script: My a-second message was '%s'" % secmsg
serverConn.disconnect()
print "@Script: Succesfully disconnected from server!"
