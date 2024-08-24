/* Programma Prolog per risolvere il problema del giro del cavallo usando l'algoritmo di Warnsdorff-Squirrel con ricorsione in coda. */

main :-
    leggi_dimensione_scacchiera(Dimensione),
    leggi_posizione(Dimensione, InizioX, InizioY),
    statistics(runtime, [TempoInizio|_]),  % Inizia a monitorare il tempo di esecuzione
    write('Attendere la soluzione...'), nl,
    (   risolvi(Dimensione, (InizioX, InizioY), TempoInizio, ScacchieraFinale)
    ->  stampa_scacchiera(ScacchieraFinale)
    ;   write('Soluzione non trovata.'), nl
    ).

/* Risolve il problema del giro del cavallo.
   - Dimensione: dimensione della scacchiera.
   - PosizioneIniziale: posizione iniziale del cavallo (InizioX, InizioY).
   - TempoInizio: tempo di inizio esecuzione.
   - ScacchieraFinale: la scacchiera finale con il percorso del cavallo.
*/
risolvi(Dimensione, PosizioneIniziale, TempoInizio, ScacchieraFinale) :-
    inizializza_scacchiera(Dimensione, Scacchiera),
    algoritmo_warnsdorff(Dimensione, Scacchiera, PosizioneIniziale, 1, TempoInizio, ScacchieraFinale).

/* Implementa l'algoritmo Warnsdorff-Squirrel con ricorsione in coda.
   - Dimensione: dimensione della scacchiera.
   - Scacchiera: scacchiera corrente.
   - (X, Y): posizione corrente del cavallo.
   - Mossa: numero della mossa corrente.
   - TempoInizio: tempo di inizio esecuzione.
   - ScacchieraFinale: la scacchiera finale con il percorso completato.
*/
algoritmo_warnsdorff(Dimensione, Scacchiera, (X, Y), Mossa, TempoInizio, ScacchieraFinale) :-
    statistics(runtime, [TempoAttuale|_]),  % Ottiene il tempo corrente
    TempoTrascorso is TempoAttuale - TempoInizio,
    (TempoTrascorso > 120000 ->
        format('Tempo limite superato: ~w ms\n', [TempoTrascorso]),
        halt  % Interrompe l'esecuzione se supera 120 secondi (120000 ms)
    ; aggiorna_scacchiera(Scacchiera, (X, Y), Mossa, ScacchieraAggiornata),
        (   Mossa =:= Dimensione * Dimensione  % Caso base: tutte le celle sono state visitate
        ->  ScacchieraFinale = ScacchieraAggiornata
        ;   findall((NX, NY), 
                    (mosse_cavallo((X, Y), (NX, NY)), 
                     mossa_valida(Dimensione, ScacchieraAggiornata, (NX, NY))
                    ), 
                    Mosse),
            ordina_mosse(Dimensione, ScacchieraAggiornata, Mosse, MosseOrdinate),
            member(ProssimaMossa, MosseOrdinate),
            algoritmo_warnsdorff(Dimensione, ScacchieraAggiornata, ProssimaMossa,
                                 Mossa + 1, TempoInizio, ScacchieraFinale)
        )
    ).
/* Legge e valida la dimensione della scacchiera. 
*/
leggi_dimensione_scacchiera(Dimensione) :-
    write('Inserisci la dimensione della scacchiera (intero compreso tra 5 e 70): '),
    read(Ingresso),
    (   integer(Ingresso), Ingresso >= 5, Ingresso =< 70 ->
        Dimensione = Ingresso
    ;   (integer(Ingresso) ->
            write('Dimensione non valida. Deve essere compresa tra 5 e 70.')
        ;   write('Dato inserito non valido. Inserisci un numero intero.')
        ), nl, leggi_dimensione_scacchiera(Dimensione)
    ).

/* Legge e valida la posizione iniziale del cavallo. 
*/
leggi_posizione(N, InizioX, InizioY) :-
    repeat,
    write('Inserisci la posizione di partenza del cavallo (X,Y)'),
    format(' (X e Y interi compresi tra 0 e ~d): ', [N-1]),
    catch(read_term(user_input, Ingresso, []), _, fallimento),
    (   analizza_ingresso(Ingresso, N, InizioX, InizioY)
    ->  !
    ;   fallimento).

/* Funzione di fallimento chiamata in caso di errore durante la lettura 
*/
fallimento :-
    write('Dato inserito non valido o errore di sintassi. Riprova.\n'),
    fail.

/* Analizza il dato in ingresso e verifica che sia nel formato corretto (X,Y). 
*/
analizza_ingresso((X,Y), N, X, Y) :-
    integer(X), integer(Y), X >= 0, X < N, Y >= 0, Y < N.
analizza_ingresso(_, _, _, _) :-
    fail.

/* Inizializza la scacchiera con valori di default (-1). 
*/
inizializza_scacchiera(N, Scacchiera) :-
    length(Scacchiera, N), % Crea una lista di N righe
    maplist(crea_riga(N), Scacchiera).

/* Crea una riga della scacchiera con valori di default (-1). 
*/
crea_riga(N, Riga) :-
    length(Riga, N), % Crea una riga di lunghezza N
    maplist(=( -1), Riga). % Inizializza tutte le celle a -1

/* Genera le mosse possibili del cavallo a partire da una posizione. 
*/
mosse_cavallo((X, Y), (NuovoX, NuovoY)) :-
    member((DeltaX, DeltaY), [(2, 1), (1, 2), (-1, 2), (-2, 1),
                              (-2, -1), (-1, -2), (1, -2), (2, -1)]),
    NuovoX is X + DeltaX,
    NuovoY is Y + DeltaY,
    NuovoX >= 0,
    NuovoY >= 0.

/* Controlla se una mossa è valida (ossia se è dentro i confini della scacchiera e la cella non è  
   stata visitata). 
*/
mossa_valida(N, Scacchiera, (X, Y)) :-
    X >= 0, X < N, Y >= 0, Y < N,
    nth0(X, Scacchiera, Riga),
    nth0(Y, Riga, -1).

/* Calcola l'accessibilità di una posizione (numero di mosse valide possibili). 
*/
calcola_accessibilita_con_posizione(N, Scacchiera, (X, Y), Grado-(X, Y)) :-
    findall((NuovoX, NuovoY), (mosse_cavallo((X, Y), (NuovoX, NuovoY)), 
             mossa_valida(N, Scacchiera, (NuovoX, NuovoY))), Mosse),
    length(Mosse, Grado).

/* Ordina le mosse in base all'accessibilità (secondo la regola di Warnsdorff). 
*/
ordina_mosse(N, Scacchiera, Mosse, MosseOrdinate) :-
    maplist(calcola_accessibilita_con_posizione(N, Scacchiera), Mosse, MosseConAccessibilita),
    keysort(MosseConAccessibilita, MosseOrdinateConAccessibilita),
    estrai_valori(MosseOrdinateConAccessibilita, MosseOrdinate).

/* Estrae i valori da una lista di coppie chiave-valore. 
*/
estrai_valori([], []).
estrai_valori([_-Val|Resto], [Val|ValoriResto]) :-
    estrai_valori(Resto, ValoriResto).

/* Aggiorna la scacchiera con una nuova mossa. 
*/
aggiorna_scacchiera(Scacchiera, (X, Y), Mossa, NuovaScacchiera) :-
    (X >= 0, Y >= 0 ->  
        nth0(X, Scacchiera, Riga),  
        aggiorna_lista(Riga, Y, Mossa, NuovaRiga),  
        aggiorna_lista(Scacchiera, X, NuovaRiga, NuovaScacchiera)
    ;   throw(error(domain_error(not_less_than_zero, (X, Y)), _))).

/* Aggiorna una lista con un nuovo elemento in una posizione specifica. 
*/
aggiorna_lista([_|Resto], 0, Elem, [Elem|Resto]) :- !.
aggiorna_lista([Testa|Resto], Indice, Elem, [Testa|NuovoResto]) :-
    (Indice > 0 ->  
        NuovoIndice is Indice - 1,
        aggiorna_lista(Resto, NuovoIndice, Elem, NuovoResto)
    ;   throw(error(domain_error(not_less_than_zero, Indice), _))).

/* Stampa la scacchiera. 
*/
stampa_scacchiera(Scacchiera) :-
    stampa_righe(Scacchiera).

/* Stampa tutte le righe della scacchiera. 
*/
stampa_righe([]).
stampa_righe([Riga|Resto]) :-
    stampa_riga(Riga),
    nl,
    stampa_righe(Resto).

/* Stampa una singola riga della scacchiera. 
*/
stampa_riga([]).
stampa_riga([Elemento|Resto]) :-
    format('~d ', [Elemento]),  % Stampa l'elemento con uno spazio
    stampa_riga(Resto).
