from socket import *
 
HOST = 'localhost'
PORT = 10000
ADDR = (HOST,PORT)
BUFSIZE = 4096
  
if (__name__ == "__main__"):
    # open the connection to the server
    clientsocket = socket(AF_INET,SOCK_STREAM)
    clientsocket.connect((ADDR))

    # send data
    clientsocket.send()
    # receive data
    clientsocket.recv(BUFSIZE)
    # close socket
    clientsocket.close()

    
