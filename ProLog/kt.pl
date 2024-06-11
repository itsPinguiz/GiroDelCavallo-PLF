% Movimento del cavaliere
knight_move((X, Y), (NX, NY)) :-
    member((DX, DY), [(2, 1), (1, 2), (-1, 2), (-2, 1), (-2, -1), (-1, -2), (1, -2), (2, -1)]),
    NX is X + DX,
    NY is Y + DY.

% Controlla se una posizione è valida sulla scacchiera
valid_move(N, Board, (X, Y)) :-
    X >= 0, X < N, Y >= 0, Y < N,
    nth0(X, Board, Row),
    nth0(Y, Row, -1).

% Aggiorna la scacchiera con la nuova mossa
update_board(Board, (X, Y), Move, NewBoard) :-
    nth0(X, Board, Row, RestRows),
    nth0(Y, Row, -1, NewRow),
    nth0(Y, UpdatedRow, Move, NewRow),
    nth0(X, NewBoard, UpdatedRow, RestRows).

% Warnsdorff's Rule: ordina le mosse in base all'accessibilità
sort_moves(_, [], []).
sort_moves(N, [Move|Moves], [SortedMove|SortedMoves]) :-
    findall((NX, NY), (knight_move(Move, (NX, NY)), valid_move(N, Board, (NX, NY))), NextMoves),
    length(NextMoves, Accessibility),
    sort_moves(N, Moves, SortedMoves),
    insert_sorted((Accessibility, Move), SortedMoves, SortedMove).

insert_sorted(X, [], [X]).
insert_sorted((A1, M1), [(A2, M2)|Rest], [(A1, M1), (A2, M2)|Rest]) :- A1 =< A2.
insert_sorted(X, [Y|Rest], [Y|SortedRest]) :- insert_sorted(X, Rest, SortedRest).

% Risolvi il problema del tour del cavaliere
knight_tour(N, Board, (X, Y), Move, FinalBoard) :-
    update_board(Board, (X, Y), Move, UpdatedBoard),
    (   Move =:= N * N
    ->  FinalBoard = UpdatedBoard
    ;   findall((NX, NY), (knight_move((X, Y), (NX, NY)), valid_move(N, UpdatedBoard, (NX, NY))), Moves),
        sort_moves(N, Moves, SortedMoves),
        member((_, NextMove), SortedMoves),
        knight_tour(N, UpdatedBoard, NextMove, Move + 1, FinalBoard)
    ).

% Inizializza la scacchiera
initialize_board(N, Board) :-
    length(Board, N),
    maplist(length_(N), Board),
    maplist(maplist(=(-1)), Board).

length_(N, L) :- length(L, N).

% Stampa la scacchiera
print_board(Board) :-
    maplist(print_row, Board).

print_row(Row) :-
    maplist(format('~|~t~d~2+ '), Row),
    nl.

% Esegui il tour del cavaliere
knights_tour(N, StartX, StartY) :-
    initialize_board(N, Board),
    knight_tour(N, Board, (StartX, StartY), 1, FinalBoard),
    print_board(FinalBoard).

% Funzione principale per iniziare il tour del cavaliere
main :-
    write('Inserisci la dimensione della scacchiera: '),
    read(N),
    write('Inserisci la posizione di partenza del cavaliere (X, Y): '),
    read((StartX, StartY)),
    (knights_tour(N, StartX, StartY) ->
        true;
        write('Soluzione non trovata.'), nl
    ).
