% OUTPUT FILE

% This file contains the predicates to writes the solution in to a file 
% toFile/2 escribe los terminos Q y A en el archivo correspondiente, si la salida a archivo esta habilitada en la configuracion actual.
toFile(Q, A) :-
    to_file(yes),
    output_file(OutF),
    open(OutF, write, T),
    write(T, Q),
    write(T, '\n'),
    write(T, A),
    close(T).
toFile(_, _) :- to_file(no).

% Load a file without throwing an exception if it doesn't exist.
% load_file/1 carga un archivo; en caso de que se produzca una excepcion, escribe un mensaje y falla.

load_file(File) :-
    string_concat(File,'.delp', DeLPFile),
    catch([DeLPFile], _, no_file_msg(DeLPFile)),
    verify_Pi.	
