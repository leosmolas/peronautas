% This file contains those common predicates used to draw swi and xml trees.
% It is used tree_swi and tree_xml

% FOR TREE DRAWING PURPOSES

% toArrow/2 formatea las reglas, pasando de la notacion por functor (d_rule o 
% s_rule) a la notacion con flecha (-< o <-, respectivamente)
toArrow([], []) :- !.

toArrow([d_rule(Head, Body) | MoreRules], [Head -< Body | MoreArrowRules]) :-
    toArrow(MoreRules, MoreArrowRules), 
    !.

toArrow([s_rule(Head, true) | MoreRules], [Head | MoreArrowRules]) :-
    toArrow(MoreRules, MoreArrowRules),
    !.

toArrow([s_rule(Head, Body) | MoreRules], [Head <- Body | MoreArrowRules]) :-
    toArrow(MoreRules,MoreArrowRules).

% In order to draw the trees correctly, every node has a particular ID.
:- dynamic nodeID/2, xml_nodeID/2.

nodeID(node, 0).

xml_nodeID(node, 0).

newNodeID(Node,NodeIDplus) :-
    nodeID(_,NodeID), % Obtains the last ID (asserta/1 is used in each entry).
    !,
    NodeIDplus is NodeID + 1,
    asserta(nodeID(Node, NodeIDplus)).

xml_newNodeID(Node,NodeIDplus) :-
    xml_nodeID(_,NodeID), % Obtains the last ID (asserta/1 is used in each entry).
    !,
    NodeIDplus is NodeID + 1,
    asserta(xml_nodeID(Node, NodeIDplus)).
