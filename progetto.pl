:-abolish(class/3).
:-abolish(ist/2).
:-abolish(ist/3).
ist(err,_,_).
ist(err,_).

define_class(Cname,[],Slots):-
    %%controllo se è ben formata?
    assert(class(Cname,[object],Slots)),
    !.

define_class(Cname,Parents,Slots):-
    assert(class(Cname,Parents,Slots)).

new(Iname,Cname,Slots):-
    is_class(Cname),
    %ist è l'istanza vera e propria
    assert(ist(Iname,Cname,Slots)).

new(Iname,Cname):-
    is_class(Cname),
    %ist è l'istanza vera e propria
    assert(ist(Iname,Cname)).

is_class(Cname):-
    class(Cname,_,_).

is_instance(Iname):-
    ist(Iname,_).

is_instance(Iname):-
    ist(Iname,_,_).
%%bugga con le maiuscole ho già provato lowercase
slot(Inst,Slotname,Res):-
    is_instance(Inst),
    ist(Inst,_,Slots),
    cerca(Slots,Slotname,Res).

cerca([Slt1|_],Slotname,Res):-
    %%slotname+=
    atom_concat(Slotname,'=',X),
    term_to_atom(Slt1,Atomslot),
    %%ottengo solo il valore da ritornare
    remove_char(Atomslot,X,Res).

cerca([_|T],Slotname,Res):-
    cerca(T,Slotname,Res).

remove_char(S,C,X) :-
    atom_concat(L,R,S),
    atom_concat(C,W,R),
    atom_concat(L,W,X).

slotx(_,[],_):-
  !.

slotx(Inst,[X|T],Res):-
  is_instance(Inst),
  ist(Inst,_,Slotint),
  cerca(Slotint,X,Res1),
  writeln(Res1),
  slotx(Inst,T,Res).
