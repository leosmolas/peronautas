%
% This file contains the predicates to build the xml tree representation used by the VigLab representation
% Is uses the files:
% - aux_tree_drwaing = which gives the basic predicates used for tree building
% - intern_config    = dynamic save_tree/1
% - aux_predicates   = multiconcat

%%%%%%%%%%%%%%%%%%%%
%%% DYNAMIC TREE %%%
%%%%%%%%%%%%%%%%%%%%

:- dynamic possibleAttack/3.
:- dynamic attacked/1.
:- dynamic xml_toRoot/1, xml_toTree/4.
:- dynamic xml_tree_file/1.

%%%%%%%%%%%%%%%%%%%%%%%
%%%% TREE DRAWING  %%%%
%%%%%%%%%%%%%%%%%%%%%%%
:-[aux_tree_drawing].


%GETCONCLUSION/2 takes a list of rules and returns the conclusion Conc of the first rule
%(chequear que esta conclusion no es siempre la que esta siendo soportada por el argumento)
getConclusion([d_rule(Conc,_)|_],Conc):-!.
getConclusion([s_rule(Conc,_)|_],Conc).


%GETABBREVLABEL/2 takes a list of rules (ie, an argument) and returns the 'argument structure'
%of the form '<A,h>'
getAbbrevLabel(Arg,Label):-
        getConclusion(Arg,Conc),
        term_to_atom(Conc,AtomicConc),
        
        getArgID(Arg,ArgID),

        multi_concat(['&amp;lt;',ArgID,',',AtomicConc,'&amp;gt;'],Label).
        %multi_concat([ArgID,',',AtomicConc],Label).


:- dynamic argID/2,currentArgID/1.

currentArgID('A'). %...this fact holds the current argument ID

getArgID(Arg,ArgID):- %...if the argument already has an ID
        argID(Arg,ArgID),!.

getArgID(Arg,ArgID):- %...if the argument has no ID yet, a new one is generated and assigned
        nextArgID(ArgID),
        assert(argID(Arg,ArgID)).


:- dynamic currentArgNumID/1.

currentArgNumID(1). %...this fact holds the current argument numeric-ID (just in case the last char-ID is reached)
lastCharCode(LC):-char_code('Z',LC). %...this fact holds the last char-ID code

nextArgID(ArgID):- %...creates char-IDs from 'A' to 'Z'
        currentArgID(ArgID),

        char_code(ArgID,Code),
        lastCharCode(LastCode),
        Code =< LastCode,!,

        NewCode is Code + 1,
        char_code(NewArgID,NewCode),

        retract(currentArgID(_)),
        assert(currentArgID(NewArgID)).

nextArgID(ArgID):- %...creates alphanumeric IDs starting from 'Z1'
        lastCharCode(LastCode),
        char_code(LastChar,LastCode),

        currentArgNumID(NumID),
        concat(LastChar,NumID,ArgID),

        NewNumID is NumID + 1,
        retract(currentArgNumID(_)),
        assert(currentArgNumID(NewNumID)).
                

%...dynamic predicates used to generate the .xml output tree file...
:- dynamic xml_node/2, xml_root/1, xml_sentence/2, mark/2, notTheFirst/0.


escapeChars(Sentence,EscapedS):-
    char_code('<',LessCode),
    char_code('>',GreatCode),
    
    string_to_atom(String,Sentence),
    string_to_list(String,List),

    replace(LessCode,"&amp;lt;",List,NoLessStr),
    replace(GreatCode,"&amp;gt;",NoLessStr,EscStrNonFlat),

    flatten(EscStrNonFlat,EscStr),

    string_to_atom(EscStr,EscapedS).


%replaces an element of the list by other, and returns the resulting list
%very useful with PROLOG strings...
replace(_,_,[],[]):-!.

replace(E,R,[E|Rest],[R|Restplace]):-replace(E,R,Rest,Restplace),!.

replace(E,R,[Other|Rest],[Other|Restplace]):-replace(E,R,Rest,Restplace).



%TOROOT/1 construye parte de la sentencia xml correspondiente a la estructura de argumento
%instanciada en su unico parametro; eg: de la sentencia <node id="1" label="Jeff" status="defeated">,
%construye <node id="1" label="Jeff" - la parte de la sentencia correspondiente al status solo puede
%ser definida luego de construido todo el arbol
xml_toRoot(Arg):-
        save_tree(yes),
        xml_newNodeID(Arg,ArgID),

        toArrow(Arg,ArrowArg),
        term_to_atom(ArrowArg,AtomicArg),

        escapeChars(AtomicArg,ArgFullLabel),

        %getConclusion(Arg,Conc), %abbreviated form of the argument
        %term_to_atom(Conc,Label),

        getAbbrevLabel(Arg,Label),

        closePrevGraph,
        
        multi_concat(['    <node id="',ArgID,'" label="',Label,'" fullLabel="',ArgFullLabel,'" tooltip="',ArgFullLabel,'"'],XML_node),
        assert(xml_sentence(node(ArgID),XML_node)),

        multi_concat(['<!-- Argument: ',AtomicArg,' -->'],XML_comment),
        assert(xml_sentence(noID,XML_comment)),

        assert(xml_sentence(noID,'     <att>      <graph>')),
        
        resetNodeCount,xmlizeArg(Arg,ArgID,_),

        assert(xml_sentence(noID,'      </graph>     </att>    </node>')),
        
        assert(xml_root(ArgID)),!.

        
xml_toRoot(_):-save_tree(no),!.

xml_toRoot(ARG):-write('Error writing XML tree info about root '),write(ARG),nl.


%toRoot(_):-write(torootfailed),nl,get_char(_).


%TOTREE/2 recibe dos estructuras de argumento A1 y A2, y construye las sentencias xml correspondientes 
%al nuevo nodo que contiene a A2, asi como el arco que va desde el nodo que contiene a A1 hasta el nodo
%que contiene a A2; eg(arco): <edge source="4" target="5"/>
%AHORA RECIBE TAMBIEN EL PUNTO DE ATAQUE Y EL STATUS DEL ARCO
xml_toTree(Arg,Def,AttackedPoint,Edge_Status):-
        save_tree(yes),
            
        xml_nodeID(Arg,ArgID),!, %existing node
        xml_newNodeID(Def,DefID), %a new one

        %%%toArrow(Arg,ArrowArg),
        toArrow(Def,ArrowDef),
        
        %%%apd(ArrowArg,ArrowDef,AttackedPoint2),

        %getConclusion(Def,DefConc),
        getAbbrevLabel(Def,Label), %abreviated form of the argument
        
        term_to_atom(ArrowDef,AtomicDef),
        escapeChars(AtomicDef,EscapedDef),

        multi_concat(['    <node id= "',DefID,'" label= "',Label,'" fullLabel="',EscapedDef,'" tooltip="',EscapedDef,'"'],XML_node),
        assert(xml_sentence(node(DefID),XML_node)),
        
        multi_concat(['<!-- Argument: ',AtomicDef,' -->'],XML_comment),
        assert(xml_sentence(noID,XML_comment)),
        assert(xml_sentence(noID,'     <att>      <graph>')),
        
%%%set_edge_status(Arg,Def,AttackedPoint,Edge_Status), %%% no longer needed (done in delp.pl)

    %XMLIZEARG
    resetNodeCount,xmlizeArg(Def,DefID,SrcNodeID), 
        
        assert(xml_sentence(noID,'      </graph>     </att>    </node>')),

    possibleAttack(ArgID,AttackedPoint,AttPtID),!,
    assert(attacked(AttPtID)),
    (Edge_Status = proper -> XML_Status = 'Proper'
    ;
    XML_Status = 'Blocking'
    ),

        multi_concat(['    <edge source="',SrcNodeID,'" target="',AttPtID,'" status ="',XML_Status,'"/>'],XML_edge),
        assert(xml_sentence(noID,XML_edge)),

    assert(xml_node(ArgID,DefID)),!.

xml_toTree(_,_,_,_):-save_tree(no),!.

xml_toTree(_,Def,_,_):-write('Error writing XML tree info about defeater '),write(Def),nl.

%toTree(_,_):-write(totreefailed),nl,get_char(_).


%...if the disagreement subargument of the argument under attack is worse than the defeating argument,
%...then the status of the edge linking those arguments is 'Proper'; else, it is 'Blocking'...
%set_edge_status(A,D,Apoint,'Proper'):-
%        disagreement_subarg(A,Apoint,SubA),
%        better(arg(D,_),arg(SubA,_)),!.
%set_edge_status(_,_,_,'Blocking').


disagreement_subarg(A,Apoint,A):- %...this predicate may need further testing
        (A = [d_rule(Apoint,_)|_]
        ;
        A = [s_rule(Apoint,_)|_])
        ,!.

disagreement_subarg(A,Apoint,SubA):-
        (A = [d_rule(Conc,_)|Rest]
        ;
        A = [s_rule(Conc,_)|Rest]),
        Conc \= Apoint,
        disagreement_subarg(Rest,Apoint,SubA).


xmlizeArg([],_,_):-retractall(litID(_,_)),!.
%ArgID is used to create its nodes' IDs
xmlizeArg([Rule|Rules],ArgID,SrcNodeID):-
    xmlizeRule(Rule,ArgID,SrcNodeID), %SrcNodeID is instantiated by the first rule of the argument
    xmlizeArg(Rules,ArgID,_). %the anonymous variable ensures that SrcNodeID is instantiated by the 1st rule of the arg


xmlizeRule(Rule,ArgID,HEAD_ID):-
    (%whether the edge's status is 'Defeasible' or 'Strict'%
    Rule = d_rule(HEAD,BODY),Status = 'Defeasible',!
    ;
    Rule = s_rule(HEAD,BODY),Status = 'Strict'
    ),
    
    (
    litID(HEAD,HEAD_ID),! %if the HEAD lit of the rule already has an ID, no node is created%
    ;
    newLitsNodeID(ArgID,HEAD_ID),
    xmlizeLits(ArgID,[HEAD],HEAD_ID,head)
    ),
    toList(BODY,BLIST),
    (
    not((Status = 'Strict',BLIST = [true])),!,
    insertCommas(BLIST,BODYwithCOMMAS),
    newLitsNodeID(ArgID,BODY_ID),
    xmlizeLits(ArgID,BODYwithCOMMAS,BODY_ID,body),
    litID(HEAD,HLIT_ID),
    assert(possibleAttack(ArgID,HEAD,HLIT_ID)),
    multi_concat(['        <edge source= "',BODY_ID,'" target="',HLIT_ID,'" status ="',Status,'"/>'],XML_EDGE),
    assert(xml_sentence(noID,XML_EDGE))
    ;
    true).

insertCommas([Lit],[Lit]):-!.
insertCommas([Lit|MoreLits],[Lit,comma|MoreLitsWithCommas]):-
    insertCommas(MoreLits,MoreLitsWithCommas).


xmlizeLits(ArgID,Lits,LitsID,LitsType):-
    resetLitCount,
    term_to_atom(Lits,AtomicLits),
    multi_concat(['        <node id= "',LitsID,'" label= "',AtomicLits,'" fullLabel="',AtomicLits,'" tooltip="',AtomicLits,'">'],XML_NODE),
    assert(xml_sentence(noID,XML_NODE)),
    assert(xml_sentence(noID,'         <att>')),
    assert(xml_sentence(noID,'           <graph>')),

    xmlizeLitsAsNodes(ArgID,Lits,LitsID,LitsType),

    assert(xml_sentence(noID,'           </graph>')),
    assert(xml_sentence(noID,'         </att>')),
    assert(xml_sentence(noID,'        </node>')).


xmlizeLitsAsNodes(_,[],_,_):-!.
xmlizeLitsAsNodes(ArgID,[Lit|MoreLits],NodeID,LitsType):-
    newLitID(Lit,NodeID,LitID),
    (
    LitsType = head,! %%%,assert(possibleAttack(ArgID,Lit,LitID))
    ;
    LitsType = body
    ),
    (
    Lit = comma,!,
    multi_concat(['             <node id="',LitID,'" label="," fullLabel="," tooltip=","'],XML_LIT)
    ;
    term_to_atom(Lit,AtomicLit),
    multi_concat(['             <node id="',LitID,'" label="',AtomicLit,'" fullLabel="',AtomicLit,'" tooltip="',AtomicLit,'"'],XML_LIT)
    ),

    assert(xml_sentence(lit(LitID),XML_LIT)),
    xmlizeLitsAsNodes(ArgID,MoreLits,NodeID,body).


:-dynamic litID/2,litCount/1.
litCount(0).
resetLitCount:-retract(litCount(_)),assert(litCount(0)).
newLitID(Lit,NodeID,LitID):-
    litCount(Count),
    LitID is NodeID*100+Count,

    asserta(litID(Lit,LitID)),

    NewCount is Count+1,
    retract(litCount(_)),
    assert(litCount(NewCount)).

:- dynamic nodeCount/1.
nodeCount(0).
resetNodeCount:-retract(nodeCount(_)),assert(nodeCount(0)).
newLitsNodeID(ArgID,ID):-
    nodeCount(Count),
    ID is ArgID*100+Count,

    NewCount is Count+1,
    retract(nodeCount(_)),
    assert(nodeCount(NewCount)).

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% AUX %%% AUX %%% AUX %%% AUX %%% AUX %%% AUX %%% AUX %%% AUX %%% AUX %%% AUX %%% AUX %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%TOLIST/2 converts its first parameter (the body of a rule) to a prolog list
toList(','(X,Y),L):-
        toList(Y,Yt),
        append([X],Yt,L),!.
toList(X,[X]):-atom(X),!.
toList(~X,[~X]):-atom(X),!.
toList(X,[X]):-compound(X).


%CLOSELASTGRAPH chequea si el grafo actual no es el primero; en ese caso, cierra el anterior
closePrevGraph:-
        notTheFirst,
        closeGraph,
        assert(xml_sentence(noID,'  <graph>')),!.

closePrevGraph:-assert(notTheFirst).


%OPENTREE/0 vacia el archivo que contiene la info para graficar los arboles si
%dicha opcion esta habilitada en la configuracion actual
xml_openTree:-
          save_tree(yes),
          %xml_tree_file(TFile),
          %open(TFile,write,T),
          %write(T,''),
          %close(T),
          retractall(xml_sentence(_,_)),
          assert(xml_sentence(noID,'<forest>')),
          assert(xml_sentence(noID,'  <graph>')).
          
xml_openTree:-save_tree(no).


%DUMPTREE/0 vacia el archivo .xml y segun los hechos dinamicos (ordenados) xml_sentence/2,
%escribe en el archivo .xml cada una de esas sentencias
xml_dumpTree:-
        save_tree(yes),
        closeGraph, %el ultimo tag <graph> no esta cerrado, porque no hay proximo grafo que lo cierre
        assert(xml_sentence(noID,'</forest>')),
        mark_tree,
        %%%tree_file(TFile),
        %%%open(TFile,write,T),
        %%%write(T,''), %debera ser redundante
        %%%dumpXML(T),
        %%%close(T).
    getXML(XML),
    retractall(explanation_list(_)),
    retractall(notTheFirst),
    assert(explanation_list(XML)).

xml_dumpTree:-save_tree(no).


%CLOSEGRAPH/0 escribe la etiqueta XML de cierre de un grafo
closeGraph :-
        save_tree(yes),
        assert(xml_sentence(noID,'  </graph>')).

closeGraph:-save_tree(no).


%MARK_TREE/0 recorre cada arbol generado y determina el status de cada uno de sus nodos
mark_tree:-
        forall(
        xml_root(NodeID),

        mark_sub_tree(NodeID,_)
        ).


%MARK_SUB_TREE/2 revisa el status de los hijos del nodo correspondiente a su primer
%parametro y, si todos estan 'defeated', lo marca como 'undefeated', instanciando
%el segundo argumento con esa etiqueta; en otro caso, lo marca como 'defeated'
mark_sub_tree(SubRootID,undefeated):-
        forall(
                xml_node(SubRootID,SonID),

                (mark_sub_tree(SonID,Mark),Mark = defeated)),assert(mark(SubRootID,'Undefeated')),!.

mark_sub_tree(NodeID,defeated):-assert(mark(NodeID,'Defeated')).


%DUMPXML/1 recibe el handler de un archivo en el que se escribiran las sentencias XML
%y, para cada sentencia, la escribe en el archivo segun corresponda
dumpXML(File):-
        forall(xml_sentence(ID,Sentence),dump(ID,Sentence,File)).

%GETXML/1 idem DUMPXML/1, pero instancia XMLdata con un string con la info codificada
getXML(XMLdata):-
    findall(CompleteSentence,(xml_sentence(ID,Sentence),getXMLsentence(ID,Sentence,CompleteSentence)),XMLdata).


%DUMP/3 recibe una etiqueta, una sentencia y un nombre de archivo y, segun si la
%etiqueta se corresponde con la ID de un nodo o no, completa el status de la
%sentencia XML correspondiente a ese nodo (con su status) o no; y, a continuacion,
%escribe la sentencia en el archivo
dump(noID,Sentence,File):-write(File,Sentence),!.

dump(lit(ID),Sentence,File):-
    attacked(ID),!,
    multi_concat([Sentence,' status="Attacked"/>'],CompleteSentence),
    write(File,CompleteSentence).

dump(lit(_ID),Sentence,File):-
    multi_concat([Sentence,' status="Free"/>'],CompleteSentence),
    write(File,CompleteSentence),!.

dump(node(ID),Sentence,File):-
        mark(ID,Mark),
        multi_concat([Sentence,' status=','\"',Mark,'\">'],CompleteSentence),
        write(File,CompleteSentence).

%GETXMLSENTENCE/3 idem DUMPXML/1, pero instancia la ultima variable con el string
%correspondiente a la sentencia XML
getXMLsentence(noID,Sentence,Sentence):-!.

getXMLsentence(lit(ID),Sentence,CompleteSentence):-
    attacked(ID),!,
    multi_concat([Sentence,' status="Attacked"/>'],CompleteSentence),!.

getXMLsentence(lit(_ID),Sentence,CompleteSentence):-
    multi_concat([Sentence,' status="Free"/>'],CompleteSentence),!.

getXMLsentence(node(ID),Sentence,CompleteSentence):-
        mark(ID,Mark),
        multi_concat([Sentence,' status=','\"',Mark,'\">'],CompleteSentence).


%LOAD_CONFIG
%xml_load_config:- config_file(CfgFile),ensure_loaded(CfgFile),load_default_config,treeFile2XML.

%treeFile2XML:-
%        xml_tree_file(TF),
%        concat(TF,'.xml',XML_treefile),
%        set_xml_tree_file(XML_treefile).

%%% XML TREEs %%%
:- dynamic explanation_list/1.

explanation_list([]).

xml_explanation(EXPLANATION):-
    explanation_list(LIST),
    multi_concat(LIST,EXPLANATION).






%EOF
