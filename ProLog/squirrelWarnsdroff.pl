/* Movimento del cavallo: 
- prende come argomento una posizione (X, Y);
- restituisce una nuova posizione (NX, NY) risultante da un movimento a "L". */
mossa_cavallo((X, Y), (NX, NY)) :-
    member((DX, DY), [(2, 1), (1, 2), (-1, 2), (-2, 1), (-2, -1), (-1, -2), (1, -2), (2, -1)]),
    NX is X + DX,
    NY is Y + DY.

/* Controlla se una posizione è valida sulla scacchiera:
- il primo argomento è la dimensione N della scacchiera;
- il secondo argomento è la scacchiera stessa;
- il terzo argomento è la posizione (X, Y) da verificare. */
mossa_valida(N, Scacchiera, (X, Y)) :-
    X >= 0, X < N, Y >= 0, Y < N,
    nth0(X, Scacchiera, Riga),
    nth0(Y, Riga, -1).

/* Aggiorna la scacchiera con la nuova mossa:
- il primo argomento è la scacchiera attuale;
- il secondo argomento è la posizione (X, Y) della mossa;
- il terzo argomento è il numero della mossa;
- il quarto argomento è la nuova scacchiera aggiornata. */
aggiorna_scacchiera(Scacchiera, (X, Y), Mossa, NuovaScacchiera) :-
    nth0(X, Scacchiera, Riga, RestoRighe),
    nth0(Y, Riga, -1, NuovaRiga),
    nth0(Y, RigaAggiornata, Mossa, NuovaRiga),
    nth0(X, NuovaScacchiera, RigaAggiornata, RestoRighe).

/* Calcola l'accessibilità di una casella:
- il primo argomento è la dimensione N della scacchiera;
- il secondo argomento è la scacchiera attuale;
- il terzo argomento è la posizione (X, Y);
- il quarto argomento è il grado di accessibilità (numero di mosse possibili). */
accessibilita(N, Scacchiera, (X, Y), Grado) :-
    findall((NX, NY), (mossa_cavallo((X, Y), (NX, NY)), mossa_valida(N, Scacchiera, (NX, NY))), Mosse),
    length(Mosse, Grado).

/* Ordina le mosse in base all'accessibilità:
- il primo argomento è la dimensione N della scacchiera;
- il secondo argomento è la scacchiera attuale;
- il terzo argomento è la lista delle mosse possibili;
- il quarto argomento è la lista delle mosse ordinate per accessibilità. */
ordina_mosse(N, Scacchiera, Mosse, MosseOrdinate) :-
    maplist(accessibilita_con_posizione(N, Scacchiera), Mosse, MosseConAccessibilita),
    sort(1, @=<, MosseConAccessibilita, MosseOrdinateConAccessibilita),
    pairs_values(MosseOrdinateConAccessibilita, MosseOrdinate).

accessibilita_con_posizione(N, Scacchiera, Pos, Grado-Pos) :-
    accessibilita(N, Scacchiera, Pos, Grado).

/* Risolvi il problema del giro del cavallo:
- il primo argomento è la dimensione N della scacchiera;
- il secondo argomento è la scacchiera attuale;
- il terzo argomento è la posizione corrente (X, Y);
- il quarto argomento è il numero della mossa corrente;
- il quinto argomento è la scacchiera finale. */
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
- il primo argomento è la dimensione N della scacchiera;
- il secondo argomento è la scacchiera inizializzata con -1. */
inizializza_scacchiera(N, Scacchiera) :-
    length(Scacchiera, N),
    maplist(lunghezza_(N), Scacchiera),
    maplist(maplist(=(-1)), Scacchiera).

lunghezza_(N, L) :- length(L, N).

/* Stampa la scacchiera:
- il primo argomento è la scacchiera da stampare. */
stampa_scacchiera(Scacchiera) :-
    maplist(stampa_riga, Scacchiera),
    nl.

stampa_riga(Riga) :-
    maplist(format('~|~t~d~2+ '), Riga),
    nl.

/* Validazione della dimensione della scacchiera:
- il primo argomento è la dimensione N della scacchiera;
- restituisce true se la dimensione è valida, false altrimenti. */
dimensione_valida(N) :- integer(N), N >= 5, N =< 112.

/* Validazione della posizione:
- il primo argomento è la dimensione N della scacchiera;
- il secondo argomento è la posizione (X, Y);
- restituisce true se la posizione è valida, false altrimenti. */
posizione_valida(N, (X, Y)) :- 
    integer(X), integer(Y),
    X >= 0, X < N, 
    Y >= 0, Y < N.

/* Funzione per leggere e validare la dimensione della scacchiera dall'input. */
leggi_dimensione :-
    write('Inserisci la dimensione della scacchiera: '),
    read(Input),
    (integer(Input) ->
        N = Input,
        (dimensione_valida(N) ->
            leggi_posizione(N, StartX, StartY),
            giro_del_cavallo(N, StartX, StartY)
        ;   write('Dimensione non valida. Deve essere compresa tra 5 e 112.'), nl, leggi_dimensione)
    ;   write('Input non valido. Inserisci un numero intero.'), nl, leggi_dimensione).

/* Funzione per leggere e validare la posizione di partenza dall'input:
- il primo argomento è la dimensione N della scacchiera;
- il secondo argomento è la variabile StartX che conterrà la coordinata X della posizione iniziale;
- il terzo argomento è la variabile StartY che conterrà la coordinata Y della posizione iniziale;
- richiede all'utente di inserire una posizione valida; */
leggi_posizione(N, StartX, StartY) :-
    write('Inserisci la posizione di partenza del cavallo (X, Y): '),
    read(Input),
    (Input = (X, Y), integer(X), integer(Y) ->
        (posizione_valida(N, (X, Y)) ->
            StartX = X, StartY = Y
        ;   Max is N - 1,
            write('Posizione non valida. Deve essere compresa tra 0 e '), write(Max), write('.'), nl, leggi_posizione(N, StartX, StartY))
    ;   write('Input non valido. Inserisci una coppia di interi.'), nl, leggi_posizione(N, StartX, StartY)).

/* Funzione principale per iniziare il giro del cavallo:
- inizia richiedendo la dimensione della scacchiera e la posizione di partenza valide.
- avvia il giro del cavallo con i valori validati. */
main :-
    leggi_dimensione.
    
/* Esegui il giro del cavallo:
- il primo argomento è la dimensione N della scacchiera;
- il secondo argomento è la posizione iniziale (StartX, StartY). */
giro_del_cavallo(N, StartX, StartY) :-
    inizializza_scacchiera(N, Scacchiera),
    (   giro_cavallo(N, Scacchiera, (StartX, StartY), 1, ScacchieraFinale)
    ->  stampa_scacchiera(ScacchieraFinale)
    ;   write('Soluzione non trovata.'), nl
    ).
