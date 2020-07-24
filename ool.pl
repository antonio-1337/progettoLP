%%%% COMPONENTI GRUPPO
%%%% Suteu Antonio 847020
%%%% Michele Fontana 829658

:- dynamic class/3.
:- dynamic instance/3.

%% define_class/3
%% INPUT -> nome della classe, l'eventuale classe da ereditare
%% (può essere anche una lista vuota) ed infine gli attributi/metodi da
%% assegnare alla classe
define_class(Class_Name,Parent_Classes,Slot_Values) :-
  convert(Slot_Values,[],Slots1),
  get_parent_slots(Parent_Classes,Slots1,Slots),
  assert(class(Class_Name,Parent_Classes,Slots)).


%% gestione 'inserimento' metodi all'interno del database
mycondition([H|T],I) :-
  [A,B] = H,B = method(N,M),
  append([A,I],N,K),
  X =.. K,
  assert(X :- M),
  get_methods_instance(T,I),!.

mycondition([_|T],I) :-
  get_methods_instance(T,I).

get_methods_instance([],_).
get_methods_instance([H|T],I):-
    mycondition([H|T],I).

%% new/2
%% INPUT -> il nome  dell'instanza da creare e la classe da instanziare
%% non prevede un 'costruttore con parametri'
new(Instance,Class) :-
  new(Instance,Class,[]).

%% new/3
%% INPUT -> il nome  dell'instanza da creare, la classe da instanziare
%% ed un'eventuale inizializzazione degli attributi della classe instanziata
%% (come se fosse un costruttore con parametri)
new(Instance,Class,Slot_Values) :-
    class(Class,_,Parent_SlotValues),
    convert(Slot_Values,[],Slots),
    inherit(Parent_SlotValues,Slots,Ress),
    modify(Ress,Instance,[],Final),
    get_methods_instance(Final,Instance),
    assert(instance(Instance,Class,Final)).

%% is_class/1 -> restituisce true se il valore passato in input è una Class_Name
%% presente all'interno del database
is_class(Class):-
    class(Class,_,_).

%% is_instance/1 -> restituisce true se il valore passato in input è un'instanza
%% di una qualsiasi classe presente all'interno del database
is_instance(Instance):-
    instance(Instance,_,_).

%% is_instance/2 -> restituisce true se il valore passato in input è un'instanza
%% della classe indicata come secondo parametro
is_instance(Instance,Class):-
    instance(Instance,Class,_).

%% slot/3
%% INPUT -> nome instanza, nome attributo, variabile in cui inserire il valore
%% associato a tale attributo
slot(Instance,Slot_Name,Result):-
	instance(Instance,_,Slots),
  nth1(_,Slots,[Slot_Name,Result]).

%% slotx/3
%% INPUT -> nome instanza, lista attributi e lista variabili da riempire con i
%% relativi valori associati alla lista degli attributi indicati come secondo
%% parametro
slotx(Instance,Slots,Result):-
  instance(Instance,_,All_Slots),
  get_slot_values(Slots,All_Slots,[],Result).

%% metodo ausiliario che restituisce tutte le classi all'interno del database
get_classes([X,Y,Z]) :-
  class(X,Y,Z).

%% caso base
convert([],Curr,Res) :-
  Res = Curr.
%% caso passo
convert([H|T],Curr,Res) :-
	term_to_atom(H,At),
  split_string(At,'=','=',[A1,B1]),
  term_string(A,A1),
  term_string(B,B1),
  append(Curr,[[A,B]],New),
  convert(T,New,Res).

%% get_parent_slots metodo ausiliario usato da define_class/3 per ereditare
%% gli attributi ed i metodi della classe 'padre'
%%caso base
get_parent_slots([],Slots,Res) :-
  Res = Slots.
%%caso passo
get_parent_slots([H|T],Slots,Res) :-
    class(H,_,Parent_Slots),
    inherit(Parent_Slots,Slots,New),
    get_parent_slots(T,New,Res).

%% medodo per gestire l'implicazione in Prolog senza doverla usare in modo
%% esplicito (più specificamente gestisce dell'ereditarietà)
my_cond([H|T],Slots,Res) :-
  [A,_] = H,
  nth1(_,Slots,[A,_]),
  inherit(T,Slots,Res),!.
my_cond([H|T],Slots,Res) :-
  append(Slots,[H],New),
  inherit(T,New,Res).

%% inherit -> metodo ausiliario usato da my_cond/3 per 'ereditare' gli
%%attributi ed i metodi della classe 'padre'
%%caso base
inherit([],Slots,Res) :-
  Res = Slots.
%%caso passo
inherit([H|T],Slots,Res) :-
	my_cond([H|T],Slots,Res).

%% modify -> metodo ausiliario usato da new/3 che permette di modificare i
%% il valore associato ad un attributo/metodo
%%caso base
modify([],_,Curr,Res):-
  Res = Curr.
%%caso passo
modify([H|T],Instance,Curr,Res):-
    H = [A,B],
    term_string(B,S),
    string_substituition(100,"this",Instance,S,R),
    New = [A,R],
    append(Curr,[New],New_Curr),
    modify(T,Instance,New_Curr,Res).

%% string_substituition -> metodo ausiliario usato da modify/4 per sostituire
%% il valore vecchio associato ad un attributo con uno nuovo
%%caso base
string_substituition(0,_,_,String,Result):-
  term_string(Result,String).
%%caso passo
string_substituition(M,Pattern,New,String,Result):-
    N is M-1,
    re_replace(Pattern,New,String,Res),
    string_substituition(N,Pattern,New,Res,Result).

%% get_slot_values -> metodo ausiliario usato da slotx/3
%% caso base
get_slot_values([],_,Curr,Res) :-
  Res = Curr.
%% caso passo
get_slot_values([H|T],Slots,Curr,Res):-
	nth1(_,Slots,[H,Value]),
  append(Curr,[Value],New),
  get_slot_values(T,Slots,New,Res).
