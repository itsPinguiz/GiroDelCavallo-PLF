% Movimento del cavallo
mossa_cavallo((X, Y), (NX, NY)) :-
    member((DX, DY), [(2, 1), (1, 2), (-1, 2), (-2, 1), (-2, -1), (-1, -2), (1, -2), (2, -1)]),
    NX is X + DX,
    NY is Y + DY.

% Controlla se una posizione è valida sulla scacchiera
mossa_valida(N, Scacchiera, (X, Y)) :-
    X >= 0, X < N, Y >= 0, Y < N,
    nth0(X, Scacchiera, Riga),
    nth0(Y, Riga, -1).

% Aggiorna la scacchiera con la nuova mossa
aggiorna_scacchiera(Scacchiera, (X, Y), Mossa, NuovaScacchiera) :-
    nth0(X, Scacchiera, Riga, RestoRighe),
    nth0(Y, Riga, -1, NuovaRiga),
    nth0(Y, RigaAggiornata, Mossa, NuovaRiga),
    nth0(X, NuovaScacchiera, RigaAggiornata, RestoRighe).

% Calcola l'accessibilità di una casella
accessibilita(N, Scacchiera, (X, Y), Grado) :-
    findall((NX, NY), (mossa_cavallo((X, Y), (NX, NY)), mossa_valida(N, Scacchiera, (NX, NY))), Mosse),
    length(Mosse, Grado).

% Ordina le mosse in base all'accessibilità
ordina_mosse(N, Scacchiera, Mosse, MosseOrdinate) :-
    maplist(accessibilita_con_posizione(N, Scacchiera), Mosse, MosseConAccessibilita),
    sort(1, @=<, MosseConAccessibilita, MosseOrdinateConAccessibilita),
    pairs_values(MosseOrdinateConAccessibilita, MosseOrdinate).

accessibilita_con_posizione(N, Scacchiera, Pos, Grado-Pos) :-
    accessibilita(N, Scacchiera, Pos, Grado).

% Risolvi il problema del giro del cavallo
giro_cavallo(N, Scacchiera, (X, Y), Mossa, ScacchieraFinale) :-
    aggiorna_scacchiera(Scacchiera, (X, Y), Mossa, ScacchieraAggiornata),
    (   Mossa =:= N * N
    ->  ScacchieraFinale = ScacchieraAggiornata
    ;   findall((NX, NY), (mossa_cavallo((X, Y), (NX, NY)), mossa_valida(N, ScacchieraAggiornata, (NX, NY))), Mosse),
        ordina_mosse(N, ScacchieraAggiornata, Mosse, MosseOrdinate),
        member(ProssimaMossa, MosseOrdinate),
        giro_cavallo(N, ScacchieraAggiornata, ProssimaMossa, Mossa + 1, ScacchieraFinale)
    ).

% Inizializza la scacchiera
inizializza_scacchiera(N, Scacchiera) :-
    length(Scacchiera, N),
    maplist(lunghezza_(N), Scacchiera),
    maplist(maplist(=(-1)), Scacchiera).

lunghezza_(N, L) :- length(L, N).

% Stampa la scacchiera
stampa_scacchiera(Scacchiera) :-
    maplist(stampa_riga, Scacchiera),
    nl.

stampa_riga(Riga) :-
    maplist(format('~|~t~d~2+ '), Riga),
    nl.

% Esegui il giro del cavallo
giro_del_cavallo(N, StartX, StartY) :-
    inizializza_scacchiera(N, Scacchiera),
    (   giro_cavallo(N, Scacchiera, (StartX, StartY), 1, ScacchieraFinale)
    ->  stampa_scacchiera(ScacchieraFinale)
    ;   write('Soluzione non trovata.'), nl
    ).

% Funzione principale per iniziare il giro del cavallo
main :-
    write('Inserisci la dimensione della scacchiera: '),
    read(N),
    write('Inserisci la posizione di partenza del cavallo (X, Y): '),
    read((StartX, StartY)),
    giro_del_cavallo(N, StartX, StartY).
