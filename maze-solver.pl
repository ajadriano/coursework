parse([Y|[X|_]], Y, X).
hasbarrier(I) :- parse(I, Y, X),barrier(Y,X).
outboundsY(I) :- parse(I, Y, _),mazeSize(J, _),Y>J.
outboundsY(I) :- parse(I, Y, _),Y<1.
outboundsX(I) :- parse(I, _, X),mazeSize(_, K),X>K.
outboundsX(I) :- parse(I, _, X),X<1.
inbounds(M) :- parse(M,Y,X),mazeSize(J,I),Y=<J,X=<I.
equals([A|B],[A|B]).
contains(A, [B|_]) :- equals(A,B).
contains(A, [_|B]) :- nonvar(B),contains(A, B). 
tail([A], A).
tail([_|B], C) :- tail(B,C).
validcoordinate(M) :- hasbarrier(M),!,fail.
validcoordinate(M) :- outboundsY(M),!,fail.
validcoordinate(M) :- outboundsX(M),!,fail.
validcoordinate(_).
nextcoordinates(X,Z) :- parse(X, B, A), C is B, D is A + 1, Z = [C,D], validcoordinate(Z).      
nextcoordinates(X,Z) :- parse(X, B, A), C is B, D is A - 1, Z = [C,D], validcoordinate(Z).      
nextcoordinates(X,Z) :- parse(X, B, A), C is B + 1, D is A, Z = [C,D], validcoordinate(Z).      
nextcoordinates(X,Z) :- parse(X, B, A), C is B - 1, D is A, Z = [C,D], validcoordinate(Z).   

writelist([]).
writelist([A|B]) :- write(A),write(','),writelist(B).
solvenext([C,D],[C,D],List,[[C,D]]) :- drawmap(List).
solvenext([E,F],[C,D],FullList,[[E,F]|L]) :- nextcoordinates([E,F],Z),\+contains(Z,FullList),solvenext(Z,[C,D],FullList,L).
solve([A,B],[C,D],[[A,B]|L]) :- validcoordinate([A,B]),validcoordinate([C,D]),nextcoordinates([A,B],Z),solvenext(Z,[C,D],[[A,B]|L],L).
solve([A,B],[A,B],[[A,B]]).


drawgraphic([Y,X],_) :- hasbarrier([Y,X]), write('X '), !.
drawgraphic([Y,X],Path) :- contains([Y,X], Path), write('* '), !.
drawgraphic(_,_) :- write('- '), !.
drawcoordinate([B,A],Path) :- mazeSize(Height,Width), Y is Height - B + 1, X is Width - A + 1, drawgraphic([Y,X],Path).

drawlineheader(_,0) :- !.
drawlineheader(A,X) :- Value is A - X + 1, write(Value), write(' '), Next is X - 1, drawlineheader(A,Next).
drawline(_,0,_) :- !.
drawline(B,A,Path) :- drawcoordinate([B,A], Path), Next is A - 1, drawline(B,Next,Path).
drawrows(_,0,_,_) :- !.
drawrows(B,CurrentB,A,Path) :- Value is B - CurrentB + 1, write(Value), write(' '), drawline(CurrentB,A,Path),write('\n'),Next is CurrentB - 1, drawrows(B,Next,A,Path).
drawmap(Path) :- mazeSize(B,A), write('  '), drawlineheader(A,A), write('\n'), drawrows(B,B,A,Path).

gettail([A|B],[A|C]) :- gettail(B,C).
gettail(_,[]).



