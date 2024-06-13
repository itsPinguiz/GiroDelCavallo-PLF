/* Movimento del cavallo: 
- prende come argomento una posizione (X, Y);
- restituisce una nuova posizione (NX, NY) risultante da un movimento a "L". */
mossa_cavallo((X, Y), (NX, NY)) :-
    member((DX, DY), [(2, 1), (1, 2), (-1, 2), (-2, 1), (-2, -1), (-1, -2), (1, -2), (2, -1)]),
    NX is X + DX,
    NY is Y + DY.

/* Controlla se una posizione e' valida sulla scacchiera:
- il primo argomento e' la dimensione N della scacchiera;
- il secondo argomento e' la scacchiera stessa;
- il terzo argomento e' la posizione (X, Y) da verificare. */
mossa_valida(N, Scacchiera, (X, Y)) :-
    X >= 0, X < N, Y >= 0, Y < N,
    nth0(X, Scacchiera, Riga),
    nth0(Y, Riga, -1).

/* Aggiorna la scacchiera con la nuova mossa:
- il primo argomento e' la scacchiera attuale;
- il secondo argomento e' la posizione (X, Y) della mossa;
- il terzo argomento e' il numero della mossa;
- il quarto argomento e' la nuova scacchiera aggiornata. */
aggiorna_scacchiera(Scacchiera, (X, Y), Mossa, NuovaScacchiera) :-
    nth0(X, Scacchiera, Riga, RestoRighe),
    nth0(Y, Riga, -1, NuovaRiga),
    nth0(Y, RigaAggiornata, Mossa, NuovaRiga),
    nth0(X, NuovaScacchiera, RigaAggiornata, RestoRighe).

/* Calcola l'accessibilita' di una casella:
- il primo argomento e' la dimensione N della scacchiera;
- il secondo argomento e' la scacchiera attuale;
- il terzo argomento e' la posizione (X, Y);
- il quarto argomento e' il grado di accessibilita' (numero di mosse possibili). */
accessibilita(N, Scacchiera, (X, Y), Grado) :-
    findall((NX, NY), (mossa_cavallo((X, Y), (NX, NY)), mossa_valida(N, Scacchiera, (NX, NY))), Mosse),
    length(Mosse, Grado).

/* Ordina le mosse in base all'accessibilita':
- il primo argomento e' la dimensione N della scacchiera;
- il secondo argomento e' la scacchiera attuale;
- il terzo argomento e' la lista delle mosse possibili;
- il quarto argomento e' la lista delle mosse ordinate per accessibilita'. */
ordina_mosse(N, Scacchiera, Mosse, MosseOrdinate) :-
    maplist(accessibilita_con_posizione(N, Scacchiera), Mosse, MosseConAccessibilita),
    sort(1, @=<, MosseConAccessibilita, MosseOrdinateConAccessibilita),
    pairs_values(MosseOrdinateConAccessibilita, MosseOrdinate).

accessibilita_con_posizione(N, Scacchiera, Pos, Grado-Pos) :-
    accessibilita(N, Scacchiera, Pos, Grado).

/* Risolvi il problema del giro del cavallo:
- il primo argomento e' la dimensione N della scacchiera;
- il secondo argomento e' la scacchiera attuale;
- il terzo argomento e' la posizione corrente (X, Y);
- il quarto argomento e' il numero della mossa corrente;
- il quinto argomento e' la scacchiera finale. */
giro_cavallo(N, Scacchiera, (X, Y), Mossa, ScacchieraFinale) :-
    aggiorna_scacchiera(Scacchiera, (X, Y), Mossa, ScacchieraAggiornata),
    (   Mossa =:= N * N
    ->  ScacchieraFinale = ScacchieraAggiornata
    ;   findall((NX, NY), (mossa_cavallo((X, Y), (NX, NY)), mossa_valida(N, ScacchieraAggiornata, (NX, NY))), Mosse),
        ordina_mosse(N, ScacchieraAggiornata, Mosse, MosseOrdinate),
        member(ProssimaMossa, MosseOrdinate),
        giro_cavallo(N, ScacchieraAggiornata, ProssimaMossa, Mossa + 1, ScacchieraFinale)
    ).

/* Inizializza la scacchiera:
- il primo argomento e' la dimensione N della scacchiera;
- il secondo argomento e' la scacchiera inizializzata con -1. */
inizializza_scacchiera(N, Scacchiera) :-
    length(Scacchiera, N),
    maplist(lunghezza_(N), Scacchiera),
    maplist(maplist(=(-1)), Scacchiera).

lunghezza_(N, L) :- length(L, N).

/* Stampa la scacchiera:
- il primo argomento e' la scacchiera da stampare. */
stampa_scacchiera(Scacchiera) :-
    maplist(stampa_riga, Scacchiera),
    nl.

stampa_riga(Riga) :-
    maplist(format('~|~t~d~2+ '), Riga),
    nl.

/* Esegui il giro del cavallo:
- il primo argomento e' la dimensione N della scacchiera;
- il secondo argomento e' la posizione iniziale (StartX, StartY). */
giro_del_cavallo(N, StartX, StartY) :-
    inizializza_scacchiera(N, Scacchiera),
    (   giro_cavallo(N, Scacchiera, (StartX, StartY), 1, ScacchieraFinale)
    ->  stampa_scacchiera(ScacchieraFinale)
    ;   write('Soluzione non trovata.'), nl
    ).

/* Funzione principale per iniziare il giro del cavallo. */
main :-
    write('Inserisci la dimensione della scacchiera: '),
    read(N),
    write('Inserisci la posizione di partenza del cavallo (X, Y): '),
    read((StartX, StartY)),
    giro_del_cavallo(N, StartX, StartY).
