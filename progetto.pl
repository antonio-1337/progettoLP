:-abolish(class/3).
:-abolish(ist/2).
:-abolish(ist/3).
instance(err,_,_).
instance(err,_).

define_class(Cname,[],Slots):-
    %%controllo se è ben formata?
    assert(class(Cname,[object],Slots)),
    !.

define_class(Cname,Parents,Slots):-
    assert(class(Cname,Parents,Slots)).

new(Iname,Cname,Slots):-
    is_class(Cname),
    %ist è l'istanza vera e propria
    assert(instance(Iname,Cname,Slots)).

new(Iname,Cname):-
    is_class(Cname),
    %ist è l'istanza vera e propria
    assert(instance(Iname,Cname)).

is_class(Cname):-
    class(Cname,_,_).

is_instance(Iname):-
    instance(Iname,_).

is_instance(Iname):-
    instance(Iname,_,_).

slot(Inst,Slotname,Res):-
    is_instance(Inst),
    instance(Inst,_,Slots),
    cerca(Slots,Slotname,Res).

cerca([Slt1|_],Slotname,Res):-
    %%slotname+=
    atom_concat(Slotname,'=',X),
    term_to_atom(Slt1,Atomslot),
    %%ottengo solo il valore da ritornare
    remove_char(Atomslot,X,Res1),
    term_to_atom(Res,Res1).

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
  instance(Inst,_,Slotint),
  cerca(Slotint,X,Res1),
  writeln(Res1),
  slotx(Inst,T,Res).
