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
   git clone https://github.com/yourusername/giro-del-cavallo.git
   cd giro-del-cavallo
   ```
2. Compila il programma Haskell:
   ```bash
   ghc -o giro_cavallo haskell/giro_cavallo.hs
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
   git clone https://github.com/yourusername/giro-del-cavallo.git
   cd giro-del-cavallo
   ```
2. Carica il programma Prolog:
   ```bash
   swipl
   ?- consult('prolog/giro_cavallo.pl').
   ```
3. Esegui il programma:
   ```bash
   ?- main.
   ```
4. Inserisci la dimensione della scacchiera e la posizione di partenza del cavaliere quando richiesto.

## Struttura del Progetto

Il repository è organizzato come segue:

```bash
giro-del-cavallo/
│
├── haskell/
│   └── giro_cavallo.hs      # Implementazione Haskell
│
└── prolog/
    └── giro_cavallo.pl      # Implementazione Prolog
```
