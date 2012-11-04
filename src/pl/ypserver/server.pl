cartelito:-
	nl,nl,
	write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'),nl,
	write('%%                       Server Yellow Pages                                 %%'),nl,
	write('%%                          Localhost:8000                                   %%'),nl,
	write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'),nl,
	nl,nl,
    write('Server Running ....'),nl.

:-	consult('primitivas/ypagent.pl').
:-	start(8000), 
	cartelito.