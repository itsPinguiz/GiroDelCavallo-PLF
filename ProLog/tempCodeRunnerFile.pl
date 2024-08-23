/* Programma Prolog per risolvere il problema del giro del cavallo usando l'algoritmo di Warnsdorff-Squirrel con ricorsione in coda. */

main :-
    leggi_dimensione_scacchiera(Dimensione),
    leggi_posizione(Dimensione, InizioX, InizioY),
    write('Attendere la soluzione...'), nl,
    (   risolvi(Dimensione, (InizioX, InizioY), ScacchieraFinale)
    ->  stampa_scacchiera(ScacchieraFinale)
    ;   write('Soluzione non trovata.'), nl
    ).

/* Risolvi il giro del cavallo senza controllo del tempo:
   - Il primo argomento è la dimensione N della scacchiera;
   - Il secondo argomento è la posizione iniziale (InizioX, InizioY);
   - Il terzo argomento è la scacchiera finale. */
risolvi(N, PosizioneIniziale, ScacchieraFinale) :-
    inizializza_scacchiera(N, Scacchiera),
    algoritmo_warnsdorff(N, Scacchiera, PosizioneIniziale, 1, ScacchieraFinale).

/* Algoritmo Warnsdorff-Squirrel:
   - Il primo argomento è la dimensione N della scacchiera;
   - Il secondo argomento è la scacchiera corrente;
   - Il terzo argomento è la posizione corrente (X, Y);
   - Il quarto argomento è il numero della mossa corrente;
   - Il quinto argomento è la scacchiera finale. */
algoritmo_warnsdorff(N, Scacchiera, (X, Y), Mossa, ScacchieraFinale) :-
    aggiorna_scacchiera(Scacchiera, (X, Y), Mossa, ScacchieraAggiornata),
    (   Mossa =:= N * N
    ->  ScacchieraFinale = ScacchieraAggiornata
    ;   findall((NX, NY), 
                (mosse_cavallo((X, Y), (NX, NY)), 
                 mossa_valida(N, ScacchieraAggiornata, (NX, NY))
                ), 
                Mosse),
        ordina_mosse(N, ScacchieraAggiornata, Mosse, MosseOrdinate),
        member(ProssimaMossa, MosseOrdinate),
        algoritmo_warnsdorff(N, ScacchieraAggiornata, ProssimaMossa, Mossa + 1, ScacchieraFinale)
    ).

/* Lettura e validazione della dimensione della scacchiera dai dati in ingresso: */
leggi_dimensione_scacchiera(Dimensione) :-
    write('Inserisci la dimensione della scacchiera (intero compreso tra 5 e 112): '),
    read(Ingresso),
    (   integer(Ingresso), Ingresso >= 5, Ingresso =< 112 ->
        Dimensione = Ingresso
    ;   (integer(Ingresso) ->
            write('Dimensione non valida. Deve essere compresa tra 5 e 112.')
        ;   write('Dato inserito non valido. Inserisci un numero intero.')
        ), nl, leggi_dimensione_scacchiera(Dimensione)
    ).

/* Lettura e validazione della posizione di partenza: */
leggi_posizione(N, InizioX, InizioY) :-
    write('Inserisci la posizione di partenza del cavallo (X,Y)'),
    format(', (X e Y interi compresi tra 0 e ~d): ', [N-1]),
    read((X, Y)),
    (   integer(X), integer(Y), X >= 0, X < N, Y >= 0, Y < N ->
        (InizioX = X, InizioY = Y)
    ;   (integer(X), integer(Y) ->
            Max is N - 1,
            write('Posizione non valida. Deve essere compresa tra 0 e '), write(Max), write('.')
        ;   write('Dato inserito non valido. Inserisci una coppia di interi.')
        ), nl, leggi_posizione(N, InizioX, InizioY)
    ).

/* Inizializza la scacchiera: */
inizializza_scacchiera(N, Scacchiera) :-
    length(Scacchiera, N), % Crea una lista di N righe
    maplist(crea_riga(N), Scacchiera).

/* Crea una riga della scacchiera: */
crea_riga(N, Riga) :-
    length(Riga, N), % Crea una riga di lunghezza N
    maplist(=( -1), Riga). % Inizializza tutte le celle a -1

/* Movimento del cavallo: */
mosse_cavallo((X, Y), (NuovoX, NuovoY)) :-
    member((DeltaX, DeltaY), [(2, 1), (1, 2), (-1, 2), (-2, 1),
                              (-2, -1), (-1, -2), (1, -2), (2, -1)]),
    NuovoX is X + DeltaX,
    NuovoY is Y + DeltaY,
    NuovoX >= 0,
    NuovoY >= 0.

/* Controlla se una posizione è valida sulla scacchiera: */
mossa_valida(N, Scacchiera, (X, Y)) :-
    X >= 0, X < N, Y >= 0, Y < N,
    nth0(X, Scacchiera, Riga),
    nth0(Y, Riga, -1).

/* Calcola l'accessibilità di una posizione: */
calcola_accessibilita_con_posizione(N, Scacchiera, (X, Y), Grado-(X, Y)) :-
    findall((NuovoX, NuovoY), (mosse_cavallo((X, Y), (NuovoX, NuovoY)), 
             mossa_valida(N, Scacchiera, (NuovoX, NuovoY))), Mosse),
    length(Mosse, Grado).

/* Ordina le mosse in base all'accessibilità: */
ordina_mosse(N, Scacchiera, Mosse, MosseOrdinate) :-
    maplist(calcola_accessibilita_con_posizione(N, Scacchiera), Mosse, MosseConAccessibilita),
    keysort(MosseConAccessibilita, MosseOrdinateConAccessibilita),
    estrai_valori(MosseOrdinateConAccessibilita, MosseOrdinate).

/* Estrae i valori da una lista di coppie chiave-valore: */
estrai_valori([], []).
estrai_valori([_-Val|Resto], [Val|ValoriResto]) :-
    estrai_valori(Resto, ValoriResto).

/* Aggiorna la scacchiera con la nuova mossa: */
aggiorna_scacchiera(Scacchiera, (X, Y), Mossa, NuovaScacchiera) :-
    (X >= 0, Y >= 0 ->  
        nth0(X, Scacchiera, Riga),  
        aggiorna_lista(Riga, Y, Mossa, NuovaRiga),  
        aggiorna_lista(Scacchiera, X, NuovaRiga, NuovaScacchiera)
    ;   throw(error(domain_error(not_less_than_zero, (X, Y)), _))).

/* Aggiorna una lista con un nuovo elemento in una posizione specifica: */
aggiorna_lista([_|Resto], 0, Elem, [Elem|Resto]) :- !.
aggiorna_lista([Testa|Resto], Indice, Elem, [Testa|NuovoResto]) :-
    (Indice > 0 ->  
        NuovoIndice is Indice - 1,
        aggiorna_lista(Resto, NuovoIndice, Elem, NuovoResto)
    ;   throw(error(domain_error(not_less_than_zero, Indice), _))).

/* Stampa la scacchiera: */
stampa_scacchiera(Scacchiera) :-
    stampa_righe(Scacchiera).

/* Stampa tutte le righe della scacchiera: */
stampa_righe([]).
stampa_righe([Riga|Resto]) :-
    stampa_riga(Riga),
    nl,
    stampa_righe(Resto).

/* Stampa una singola riga della scacchiera: */
stampa_riga([]).
stampa_riga([Elemento|Resto]) :-
    format('~d ', [Elemento]),  % Stampa l'elemento con uno spazio
    stampa_riga(Resto).
