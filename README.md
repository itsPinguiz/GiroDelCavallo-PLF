# Progetto del Giro del Cavallo

Questo repository contiene due implementazioni del problema del giro del cavallo, una in Haskell e una in Prolog, utilizzando l'algoritmo di Warnsdorff-Squirrel.

## Descrizione del Problema

Il problema del giro del cavallo consiste nel trovare un percorso in cui un cavallo degli scacchi visita ogni casella di una scacchiera esattamente una volta. È un problema classico di teoria dei grafi e di backtracking.

## Algoritmi Utilizzati

### Algoritmo di Warnsdorff-Squirrel

L'algoritmo di Warnsdorff è una tecnica euristica utilizzata per risolvere il problema del giro del cavallo. L'algoritmo funziona scegliendo sempre la mossa che conduce alla casella con il minor numero di mosse successive possibili, riducendo così le possibilità di stallo. La versione combinata con l'approccio di Squirrel migliora ulteriormente le performance.

## Implementazioni

### Implementazione in Haskell

L'implementazione in Haskell utilizza una struttura dati bidimensionale (lista di liste) per rappresentare la scacchiera. Le coordinate sono rappresentate come coppie di interi.

#### Esecuzione

1. Clona il repository:
   ```bash
   git clone https://github.com/yourusername/GiroDelCavallo-PLF.git
   cd GiroDelCavallo-PLF
   ```
2. Compila il programma Haskell:
   ```bash
   ghc -o giro_cavallo Haskell/squirrelWarnsdroff.hs
   ```
3. Esegui il programma compilato:
   ```bash
   ./giro_cavallo
   ```
4. Inserisci la dimensione della scacchiera e la posizione di partenza del cavaliere quando richiesto.

### Implementazione in Prolog

L'implementazione in Prolog utilizza fatti e regole per rappresentare i movimenti del cavallo e per verificare la validità delle mosse. La scacchiera è rappresentata come una lista di liste.


#### Esecuzione

1. Clona il repository:
   ```bash
   git clone https://github.com/yourusername/GiroDelCavallo-PLF.git
   cd GiroDelCavallo-PLF
   ```
2. Carica il programma Prolog:
   ```bash
   gprolog --consult-file squirrelWarnsdroff.pl
   ```
3. Esegui il programma:
   ```bash
   ?- main.
   ```
4. Inserisci la dimensione della scacchiera e la posizione di partenza del cavaliere quando richiesto.

## Struttura del Progetto

Il repository è organizzato come segue:

```bash
GiroDelCavallo-PLF/
│
├── Haskell/
│   └── squirrelWarnsdroff.hs.    # Implementazione Haskell
│
└── Prolog/
    └── squirrelWarnsdroff.pl      # Implementazione Prolog
```

## Correzzioni segnalate dal Professore

### Lingua e Impaginazione:

- [ ] Bisogna mettere "italian" per evitare cose del tipo "min-imizza" [-1].

### Specifica del Problema:

- [ ] L'intero N può essere non positivo? Che significa "in tempi ragionevoli"? [-2].

### Analisi del Problema:

- [ ] Perché N diventa prima naturale e poi \ge 1? (x, y) che limiti ha?

- [ ] Nella matrice i numeri devono essere distinti?

- [ ] Da nessuna parte viene scritto che il cavallo si muove a L [-4].

### Progettazione dell'Algoritmo:

- [ ] Bisogna spiegare come funzionano l'algoritmo di Warnsdorff
e la strategia di Squirrel e se il primo trova sempre
la soluzione (quando esiste) come l'algoritmo di backtracking [2].

- [ ] "Ripetizione del processo ricorsivo" è un ossimoro [-1].

### Qualitá del Software:
#### Leggibile (coerenza stilistica)?
- [X] In Haskell prima di main sarebbe il caso di lasciare una linea vuota [-1].

- [X] leggiDimensioneScacchiera non è una funzione ma un'azione [-1].

- [X] Presenza di linee lunghe che andando a capo rovinano l'indentazione [-1].

- [X] In Prolog si chiamano predicati, non funzioni [-1].

- [X] Misto di identificatori in italiano e in inglese (StartX) [-1].


#### Funzionante (limitazioni/validazione/usabilità)?
- [X] In Prolog dopo "main." e "Inserisci la dimensione della scacchiera (intero compreso tra 5 e 112):" compare
"uncaught exception: error(existence_error(procedure,read_line_to_string/2),leggi_dimensione_scacchiera/0)"
[-4].

### Note:
Per Haskell dovete consegnarmi solo il .hs.
