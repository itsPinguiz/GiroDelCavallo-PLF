:- use_module(library(time)). /* per impostare un limite di tempo per l'esecuzione del programma. */

/* Programma Prolog per risolvere il problema del giro del cavallo usando l'algoritmo di Warnsdorff-Squirrel. */

main :-
    leggi_dimensione_scacchiera(Dimensione),
    leggi_posizione(Dimensione, StartX, StartY),
    write('Attendere la soluzione...'), nl,
    (   catch(call_with_time_limit(120, risolvi_giro_cavallo(Dimensione, (StartX, StartY), ScacchieraFinale)),
              time_limit_exceeded,
              (write('Tempo scaduto. Non è stata trovata una soluzione entro il tempo limite.'), nl, fail))
    ->  stampa_scacchiera(ScacchieraFinale)
    ;   write('Soluzione non trovata.'), nl
    ).

/* Funzione per leggere e validare la dimensione della scacchiera dall'input:
   - chiede all'utente di inserire un numero intero compreso tra 5 e 112;
   - valida l'input e richiede nuovamente se l'input è non valido o fuori dal range specificato;
   - restituisce la dimensione valida della scacchiera. */
leggi_dimensione_scacchiera(Dimensione) :-
    write('Inserisci la dimensione della scacchiera (intero compreso tra 5 e 112): '),
    read_line_to_string(user_input, Input),
    (   catch(number_string(N, Input), _, fail),
        integer(N), N >= 5, N =< 112 ->
        Dimensione = N
    ;   (catch(number_string(N, Input), _, false), integer(N) ->
            write('Dimensione non valida. Deve essere compresa tra 5 e 112.')
        ;   write('Input non valido. Inserisci un numero intero.')
        ), nl, leggi_dimensione_scacchiera(Dimensione)
    ).

/* Funzione per leggere e validare la posizione di partenza dall'input:
   - il primo argomento è la dimensione N della scacchiera;
   - il secondo argomento è la variabile StartX che conterrà la coordinata X della posizione iniziale;
   - il terzo argomento è la variabile StartY che conterrà la coordinata Y della posizione iniziale;
   - richiede all'utente di inserire una posizione valida. */
leggi_posizione(N, StartX, StartY) :-
    format('Inserisci la posizione di partenza del cavallo (X,Y), (X e Y interi compresi tra 0 e ~d): ', [N-1]),
    read_line_to_string(user_input, Input),
    (   catch(term_string((X, Y), Input), _, fail),
        integer(X), integer(Y), X >= 0, X < N, Y >= 0, Y < N ->
        (StartX = X, StartY = Y)
    ;   (catch(term_string((X, Y), Input), _, false), integer(X), integer(Y) ->
            Max is N - 1,
            write('Posizione non valida. Deve essere compresa tra 0 e '), write(Max), write('.')
        ;   write('Input non valido. Inserisci una coppia di interi.')
        ), nl, leggi_posizione(N, StartX, StartY)
    ).

/* Inizializza la scacchiera:
   - il primo argomento è la dimensione N della scacchiera;
   - il secondo argomento è la scacchiera inizializzata con -1. */
inizializza_scacchiera(N, Scacchiera) :-
    length(Scacchiera, N), % Crea una lista di N elementi (le righe della scacchiera)
    maplist({N}/[Riga]>>length(Riga, N), Scacchiera), % Imposta la lunghezza di ciascuna riga a N
    maplist(maplist(=(-1)), Scacchiera). % Inizializza ogni cella della scacchiera con -1

/* Movimento del cavallo: 
   - prende come argomento una posizione (X, Y);
   - restituisce una nuova posizione (NX, NY) risultante da un movimento a "L". */
mosse_cavallo((X, Y), (NX, NY)) :-
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

/* Calcola l'accessibilità di una posizione e la associa alla posizione stessa:
   - il primo argomento è la dimensione N della scacchiera;
   - il secondo argomento è la scacchiera attuale;
   - il terzo argomento è la posizione (X, Y);
   - il quarto argomento è una coppia Grado-Pos, dove Grado è il numero di mosse possibili da (X, Y) e Pos è la posizione stessa. */
calcola_accessibilita_con_posizione(N, Scacchiera, (X, Y), Grado-(X, Y)) :-
    findall((NX, NY), (mosse_cavallo((X, Y), (NX, NY)), mossa_valida(N, Scacchiera, (NX, NY))), Mosse),
    length(Mosse, Grado).

/* Ordina le mosse in base all'accessibilità:
   - il primo argomento è la dimensione N della scacchiera;
   - il secondo argomento è la scacchiera attuale;
   - il terzo argomento è la lista delle mosse possibili;
   - il quarto argomento è la lista delle mosse ordinate per accessibilità. */
ordina_mosse(N, Scacchiera, Mosse, MosseOrdinate) :-
    maplist(calcola_accessibilita_con_posizione(N, Scacchiera), Mosse, MosseConAccessibilita),
    sort(1, @=<, MosseConAccessibilita, MosseOrdinateConAccessibilita),
    pairs_values(MosseOrdinateConAccessibilita, MosseOrdinate).

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

/* Esegui il giro del cavallo:
   - il primo argomento è la dimensione N della scacchiera;
   - il secondo argomento è la posizione iniziale (StartX, StartY);
   - il terzo argomento è la scacchiera finale. */
risolvi_giro_cavallo(N, (StartX, StartY), ScacchieraFinale) :-
    inizializza_scacchiera(N, Scacchiera),
    algoritmo_warnsdorff_squirrel(N, Scacchiera, (StartX, StartY), 1, ScacchieraFinale).

/* Risolvi il problema del giro del cavallo:
   - il primo argomento è la dimensione N della scacchiera;
   - il secondo argomento è la scacchiera attuale;
   - il terzo argomento è la posizione corrente (X, Y);
   - il quarto argomento è il numero della mossa corrente;
   - il quinto argomento è la scacchiera finale. */
algoritmo_warnsdorff_squirrel(N, Scacchiera, (X, Y), Mossa, ScacchieraFinale) :-
    aggiorna_scacchiera(Scacchiera, (X, Y), Mossa, ScacchieraAggiornata),
    (   Mossa =:= N * N
    ->  ScacchieraFinale = ScacchieraAggiornata
    ;   findall((NX, NY), (mosse_cavallo((X, Y), (NX, NY)), mossa_valida(N, ScacchieraAggiornata, (NX, NY))), Mosse),
        ordina_mosse(N, ScacchieraAggiornata, Mosse, MosseOrdinate),
        member(ProssimaMossa, MosseOrdinate),
        algoritmo_warnsdorff_squirrel(N, ScacchieraAggiornata, ProssimaMossa, Mossa + 1, ScacchieraFinale)
    ).

/* Stampa la scacchiera:
   - il primo argomento è la scacchiera da stampare. */
stampa_scacchiera(Scacchiera) :-
    maplist([Riga]>>(
        maplist(format('~|~t~d~2+ '), Riga),
        nl
    ), Scacchiera),
    nl.
