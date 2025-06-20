%% star(+Commands)
% Commands is een lijst van commando's




%%%%%%%
%% 1 %%
%%%%%%%

collect_stars(Commandos, Output) :-
  % Zet accumulator op []
  collect_stars((0,0), Commandos, [], Output).


%% Tail recursieve versie voor performatie
%% collect_stars(Startpositie, Commandos, Accumulator, Resultaat)

% Unificeer accumulator met resultaat
collect_stars(_, [], UnsortedRes, Res) :- sort(UnsortedRes, Res).

% Mogelijkheden voor Head
collect_stars((CurrX, CurrY), [right | Rest], Acc, Res) :- 
  NewX is CurrX + 1,
  collect_stars((NewX, CurrY), Rest, Acc, Res).
collect_stars((CurrX, CurrY), [left | Rest], Acc, Res) :- 
  NewX is CurrX - 1,
  collect_stars((NewX, CurrY), Rest, Acc, Res).
collect_stars((CurrX, CurrY), [up | Rest], Acc, Res) :- 
  NewY is CurrY + 1,
  collect_stars((CurrX, NewY), Rest, Acc, Res).
collect_stars((CurrX, CurrY), [down | Rest], Acc, Res) :- 
  NewY is CurrY - 1,
  collect_stars((CurrX, NewY), Rest, Acc, Res).

collect_stars((CurrX, CurrY), [star | Rest], Acc, Res) :- 
  collect_stars((CurrX, CurrY), Rest, [star(CurrX, CurrY) | Acc], Res).

/*
?- collect_stars([right,right,right,right,right,star],S).
S = [star(5,0)] ;

?- collect_stars([down,star,left,down,right,up,up,star,right],S).
S = [star(0,-1),star(0,0)] ;
*/






%%%%%%%
%% 2 %%
%%%%%%%

% Hulpfunctie
writeSpaces(0) :- !.
writeSpaces(N) :-
  New is N-1,
  write(' '),
  writeSpaces(New).

%% write1line(+StarList, +Y, +StartCol)
write1line([], _, _).

write1line([star(_, OtherY) | T], Y, CurrPos) :-
  OtherY \== Y, !,
  write1line(T, Y, CurrPos).

write1line([star(X, Y) | T], Y, CurrPos) :- !,
  AmountOfSpaces is X - CurrPos,
  writeSpaces(AmountOfSpaces),
  write(*),
  NewPos is X + 1,
  write1line(T, Y, NewPos).



%%%%%%%%%%%%%%%%%
%% ALTERNATIEF %%
%%%%%%%%%%%%%%%%%

%% write1line_alt(StarList, Y, StartCol)

% Basisgeval
write1line_alt([], _, _).

write1line_alt([star(_, OtherY) | T], Y, CurrPos) :-
  OtherY \== Y, !,
  write1line_alt(T, Y, CurrPos).

write1line_alt([star(X,YS) | T], Y, CurrPos) :-
  CurrPos < X, !,
  write(' '),
  NewPos is CurrPos + 1,
  write1line_alt([star(X,YS) | T], Y, NewPos).

write1line_alt([star(_,Y) | T], Y, CurrPos) :- !,
  % Omdat de lijst gesorteerd is, weten we dat X gelijk is
  % aan CurrPos, dus we kunnen direct schrijven
  write(*),
  NewPos is CurrPos + 1,
  write1line_alt(T, Y, NewPos).






%%%%%%%
%% 3 %%
%%%%%%%

% Hulpfunctie
writeLines(Stars, 0, FirstX) :- write1line(Stars,0,FirstX), !.
writeLines(Stars, Y, FirstX) :-
  write1line(Stars, Y, FirstX), nl,
  NewY is Y - 1,
  writeLines(Stars, NewY, FirstX).

% Main functie
star(Commandos) :-
  collect_stars(Commandos, Stars),
  Stars = [star(FirstX, _) | _],
  setof(Y, member(star(_, Y), Stars), YCoordinaten),
  max_list(YCoordinaten, MaxY),
  writeLines(Stars, MaxY, FirstX).

/*

JOEPIE!

?- star([star,up,star,up,star,up,star,up,right,right,right,star,left,star,left,star,left,star,left,star,left,star,left,star,left,left,left,left,left,down,star,down,star,down,star,down,left,star,left,star,left,star,left,star,up,star,up,star,up,star,up,star,right,star,right,star,right,star]).

****     *******
*   *       *
*   *       *
*   *       *
****        *

true.
*/