/* Programma Prolog per risolvere il problema del giro del cavallo usando l'algoritmo di Warnsdorff-Squirrel con ricorsione in coda. */

main :-
    leggi_dimensione_scacchiera(Dimensione),
    leggi_posizione(Dimensione, InizioX, InizioY),
    statistics(runtime, [Inizio|_]),  % Inizia a monitorare il tempo di esecuzione
    write('Attendere la soluzione...'), nl,
    (   risolvi(Dimensione, (InizioX, InizioY), Inizio, ScacchieraFinale)
    ->  stampa_scacchiera(ScacchieraFinale)
    ;   write('Soluzione non trovata.'), nl
    ).

/* Risolve il problema del giro del cavallo con monitoraggio del tempo.
   - N: dimensione della scacchiera.
   - PosizioneIniziale: posizione iniziale del cavallo (InizioX, InizioY).
   - TempoIniziale: tempo di inizio esecuzione.
   - ScacchieraFinale: la scacchiera finale con il percorso del cavallo.
*/
risolvi(N, PosizioneIniziale, TempoIniziale, ScacchieraFinale) :-
    inizializza_scacchiera(N, Scacchiera),
    algoritmo_warnsdorff(N, Scacchiera, PosizioneIniziale, 1, TempoIniziale, ScacchieraFinale).

/* Implementa l'algoritmo Warnsdorff-Squirrel con controllo del tempo e ricorsione in coda.
   - N: dimensione della scacchiera.
   - Scacchiera: scacchiera corrente.
   - (X, Y): posizione corrente del cavallo.
   - Mossa: numero della mossa corrente.
   - TempoIniziale: tempo di inizio esecuzione.
   - ScacchieraFinale: la scacchiera finale con il percorso completato.
*/
algoritmo_warnsdorff(N, Scacchiera, (X, Y), Mossa, TempoIniziale, ScacchieraFinale) :-
    statistics(runtime, [TempoAttuale|_]),  % Ottiene il tempo corrente
    TempoPassato is TempoAttuale - TempoIniziale,
    (TempoPassato > 120000 ->
        format('Tempo limite superato: ~w ms\n', [TempoPassato]),
        halt  % Interrompe l'esecuzione se supera 120 secondi (120000 ms)
    ; aggiorna_scacchiera(Scacchiera, (X, Y), Mossa, ScacchieraAggiornata),
        (   Mossa =:= N * N  % Caso base: tutte le celle sono state visitate
        ->  ScacchieraFinale = ScacchieraAggiornata
        ;   findall((NX, NY), 
                    (mosse_cavallo((X, Y), (NX, NY)), 
                     mossa_valida(N, ScacchieraAggiornata, (NX, NY))
                    ), 
                    Mosse),
            ordina_mosse(N, ScacchieraAggiornata, Mosse, MosseOrdinate),
            member(ProssimaMossa, MosseOrdinate),
            algoritmo_warnsdorff(N, ScacchieraAggiornata, ProssimaMossa,
                                 Mossa + 1, TempoIniziale, ScacchieraFinale)
        )
    ).

/* Legge e valida la dimensione della scacchiera.
   - Dimensione: dimensione della scacchiera (N).
*/
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

/* Legge e valida la posizione iniziale del cavallo.
   - N: dimensione della scacchiera.
   - InizioX, InizioY: coordinate della posizione iniziale (X, Y).
*/
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

/* Inizializza la scacchiera con valori di default (-1).
   - N: dimensione della scacchiera.
   - Scacchiera: scacchiera inizializzata.
*/
inizializza_scacchiera(N, Scacchiera) :-
    length(Scacchiera, N), % Crea una lista di N righe
    maplist(crea_riga(N), Scacchiera).

/* Crea una riga della scacchiera con valori di default (-1).
   - N: dimensione della riga.
   - Riga: riga inizializzata con valori -1.
*/
crea_riga(N, Riga) :-
    length(Riga, N), % Crea una riga di lunghezza N
    maplist(=( -1), Riga). % Inizializza tutte le celle a -1

/* Genera le mosse possibili del cavallo a partire da una posizione.
   - (X, Y): posizione corrente del cavallo.
   - (NuovoX, NuovoY): nuova posizione generata dal movimento del cavallo.
*/
mosse_cavallo((X, Y), (NuovoX, NuovoY)) :-
    member((DeltaX, DeltaY), [(2, 1), (1, 2), (-1, 2), (-2, 1),
                              (-2, -1), (-1, -2), (1, -2), (2, -1)]),
    NuovoX is X + DeltaX,
    NuovoY is Y + DeltaY,
    NuovoX >= 0,
    NuovoY >= 0.

/* Controlla se una mossa è valida (ossia se è dentro i confini della scacchiera e la cella non è stata visitata).
   - N: dimensione della scacchiera.
   - Scacchiera: scacchiera corrente.
   - (X, Y): posizione da verificare.
*/
mossa_valida(N, Scacchiera, (X, Y)) :-
    X >= 0, X < N, Y >= 0, Y < N,
    nth0(X, Scacchiera, Riga),
    nth0(Y, Riga, -1).

/* Calcola l'accessibilità di una posizione (numero di mosse valide possibili).
   - N: dimensione della scacchiera.
   - Scacchiera: scacchiera corrente.
   - (X, Y): posizione da analizzare.
   - Grado-(X, Y): coppia grado di accessibilità e posizione.
*/
calcola_accessibilita_con_posizione(N, Scacchiera, (X, Y), Grado-(X, Y)) :-
    findall((NuovoX, NuovoY), (mosse_cavallo((X, Y), (NuovoX, NuovoY)), 
             mossa_valida(N, Scacchiera, (NuovoX, NuovoY))), Mosse),
    length(Mosse, Grado).

/* Ordina le mosse in base all'accessibilità (secondo la regola di Warnsdorff).
   - N: dimensione della scacchiera.
   - Scacchiera: scacchiera corrente.
   - Mosse: lista delle mosse possibili.
   - MosseOrdinate: lista delle mosse ordinate per accessibilità.
*/
ordina_mosse(N, Scacchiera, Mosse, MosseOrdinate) :-
    maplist(calcola_accessibilita_con_posizione(N, Scacchiera), Mosse, MosseConAccessibilita),
    keysort(MosseConAccessibilita, MosseOrdinateConAccessibilita),
    estrai_valori(MosseOrdinateConAccessibilita, MosseOrdinate).

/* Estrae i valori da una lista di coppie chiave-valore.
   - Lista: lista di coppie chiave-valore.
   - Valori: lista dei valori estratti.
*/
estrai_valori([], []).
estrai_valori([_-Val|Resto], [Val|ValoriResto]) :-
    estrai_valori(Resto, ValoriResto).

/* Aggiorna la scacchiera con una nuova mossa.
   - Scacchiera: scacchiera corrente.
   - (X, Y): posizione della mossa.
   - Mossa: numero della mossa corrente.
   - NuovaScacchiera: scacchiera aggiornata con la nuova mossa.
*/
aggiorna_scacchiera(Scacchiera, (X, Y), Mossa, NuovaScacchiera) :-
    (X >= 0, Y >= 0 ->  
        nth0(X, Scacchiera, Riga),  
        aggiorna_lista(Riga, Y, Mossa, NuovaRiga),  
        aggiorna_lista(Scacchiera, X, NuovaRiga, NuovaScacchiera)
    ;   throw(error(domain_error(not_less_than_zero, (X, Y)), _))).

/* Aggiorna una lista con un nuovo elemento in una posizione specifica.
   - Lista: lista corrente.
   - Indice: posizione da aggiornare.
   - Elem: nuovo elemento da inserire.
   - NuovaLista: lista aggiornata.
*/
aggiorna_lista([_|Resto], 0, Elem, [Elem|Resto]) :- !.
aggiorna_lista([Testa|Resto], Indice, Elem, [Testa|NuovoResto]) :-
    (Indice > 0 ->  
        NuovoIndice is Indice - 1,
        aggiorna_lista(Resto, NuovoIndice, Elem, NuovoResto)
    ;   throw(error(domain_error(not_less_than_zero, Indice), _))).

/* Stampa la scacchiera.
   - Scacchiera: scacchiera da stampare.
*/
stampa_scacchiera(Scacchiera) :-
    stampa_righe(Scacchiera).

/* Stampa tutte le righe della scacchiera.
   - Scacchiera: scacchiera da stampare riga per riga.
*/
stampa_righe([]).
stampa_righe([Riga|Resto]) :-
    stampa_riga(Riga),
    nl,
    stampa_righe(Resto).

/* Stampa una singola riga della scacchiera.
   - Riga: riga da stampare.
*/
stampa_riga([]).
stampa_riga([Elemento|Resto]) :-
    format('~d ', [Elemento]),  % Stampa l'elemento con uno spazio
    stampa_riga(Resto).

