% ---------------------------
% DeLP top-level: an example of a prolog client for the DeLP server 
% ---------------------------
% (c) 2006 Artificial Intelligence Research and Development Laboratory (LIDIA). 
% Universidad Nacional del Sur. Bahia Blanca. Argentina. e-mail: delp@cs.uns.edu.ar
% Alejandro J. Garcia - Nicolas D. Rotstein - Mariano Tucat - Guillermo
% R. Simari

% main predicates: delp/0 delp/1 and delp/2
% 


:-dynamic process/2.

:-[prolog-client].


% Estos predicados se hicieron asi por que sino el exe se muere.

a:-thread_create(win_shell(open,'delp_server.exe',minimize),_,[]),!.

b:-asserta(process([X],ServerID):-send_msg(ServerID,prog(X))).

run:-a,b,delp.
