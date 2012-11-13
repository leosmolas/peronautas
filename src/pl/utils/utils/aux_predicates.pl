% This file contains the auxiliary predicates used in the diferent modules

% MULTI_CONCAT/2 recibe, como primer parametro, una lista de atomos a 
% concatenar y devuelve en su segundo argumento, dicha concatenacion.
multi_concat([Atom], Atom) :- !.
multi_concat([AtomOne, AtomTwo], ConcattedAtoms) :-
    atom_concat(AtomOne, AtomTwo, ConcattedAtoms), !.
multi_concat([Atom | MoreAtoms], ConcattedAtoms) :-
    multi_concat(MoreAtoms, SubAtomicString),
    atom_concat(Atom, SubAtomicString, ConcattedAtoms).
