%:-initialization load_foreign_library(interface).
:-[interface].


delp:-
	serverIP(SIP),
        delp(SIP,8000). 

delp(SIP,PORT):-
	connect(ID,SIP,PORT),
	send_msg(ID,stop),halt.

serverIP(IP):-
	gethostname(HOST),
	tcp_host_to_address(HOST,IP_address),
	IP_address = ip(B1,B2,B3,B4),
	concat(B1,'.',B1DOT),
	concat(B2,'.',B2DOT),
	concat(B3,'.',B3DOT),
	concat(B1DOT,B2DOT,B12),
	concat(B12,B3DOT,B123),
	concat(B123,B4,IP).
