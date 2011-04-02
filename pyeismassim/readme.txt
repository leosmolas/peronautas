Las clases que se encuentran afuera son utilizadas (scratch that) DEBERIAN SER
UTILIZADAS para lograr comunicacion via sockets.

El modo de uso es correr en una terminal el tcpserver, y a continuacion el
script connection, que utiliza una instancia de serverConnection para enviar
mensajes (again, SCRATCH THAT) INTENTAR enviar mensajes.

En un principio noto dos problemas: el primero es que el puerto especificado
(en mi caso 1029) es cambiado por CUALQUIER OTRO (hasta ahora, un numero
grande de 5 cifras, en todos los casos que testee). Sin embargo, si cambio el
puerto 1029 por, say, 15000, LA CONEXION ES RECHAZADA.

El segundo problema es que esto ESTA RE BUGEADO.
