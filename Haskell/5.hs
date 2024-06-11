import Data.List (minimumBy, sortBy, nub)
import Data.Ord (comparing)
import Data.Maybe (isJust, catMaybes, listToMaybe)
import Text.Read (readMaybe)
import Control.Monad (when)
import Control.Concurrent (threadDelay, forkIO)
import System.Timeout (timeout)
import System.Console.ANSI (clearScreen)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

type Position = (Int, Int)
type Board = [[Int]]

-- Controlla se una posizione è valida sulla scacchiera
isValid :: Int -> Board -> Position -> Bool
isValid n board (x, y) = x >= 0 && x < n && y >= 0 && y < n && board !! x !! y == -1

-- Movimento del cavaliere
knightMoves :: [Position]
knightMoves = [(2, 1), (1, 2), (-1, 2), (-2, 1), (-2, -1), (-1, -2), (1, -2), (2, -1)]

-- Calcola l'accessibilità di una casella
accessibility :: Int -> Board -> Position -> Int
accessibility n board (x, y) = length $ filter (isValid n board) [(x + dx, y + dy) | (dx, dy) <- knightMoves]

-- Algoritmo Backtracking
backtracking :: Int -> Board -> Position -> Int -> IORef (Int, Board) -> IO (Maybe Board)
backtracking n board (x, y) move ref = do
    let missing = n * n - move
    writeIORef ref (missing, board)
    if move == n * n 
        then return (Just board)
        else do
            let nextMoves = filter (isValid n board) [(x + dx, y + dy) | (dx, dy) <- knightMoves]
            results <- mapM tryMove nextMoves
            return $ listToMaybe $ catMaybes results
  where
    tryMove pos = backtracking n (updateBoard board pos move) pos (move + 1) ref

-- Algoritmo Warnsdorff's Rule
warnsdorff :: Int -> Board -> Position -> Int -> IORef (Int, Board) -> IO (Maybe Board)
warnsdorff n board pos move ref = do
    let missing = n * n - move
    writeIORef ref (missing, board)
    if move == n * n 
        then return (Just board)
        else do
            let nextMoves = filter (isValid n board) [(fst pos + dx, snd pos + dy) | (dx, dy) <- knightMoves]
            if null nextMoves
                then return Nothing
                else do
                    let sortedMoves = sortBy (comparing (accessibility n board)) nextMoves
                    results <- mapM tryMove sortedMoves
                    return $ listToMaybe $ catMaybes results
  where
    tryMove pos = warnsdorff n (updateBoard board pos move) pos (move + 1) ref

-- Algoritmo Backtracking ottimizzato con Heuristica di Warnsdorff
backtrackingWithWarnsdorff :: Int -> Board -> Position -> Int -> IORef (Int, Board) -> IO (Maybe Board)
backtrackingWithWarnsdorff n board (x, y) move ref = do
    let missing = n * n - move
    writeIORef ref (missing, board)
    if move == n * n 
        then return (Just board)
        else do
            let nextMoves = filter (isValid n board) [(x + dx, y + dy) | (dx, dy) <- knightMoves]
            if null nextMoves
                then return Nothing
                else do
                    let sortedMoves = sortBy (comparing (accessibility n board)) nextMoves
                    results <- mapM tryMove sortedMoves
                    return $ listToMaybe $ catMaybes results
  where
    tryMove pos = backtrackingWithWarnsdorff n (updateBoard board pos move) pos (move + 1) ref

-- Aggiorna la scacchiera con la nuova mossa
updateBoard :: Board -> Position -> Int -> Board
updateBoard board (x, y) move = take x board ++
                                [take y (board !! x) ++ [move] ++ drop (y + 1) (board !! x)] ++
                                drop (x + 1) board

-- Stampa la soluzione
printBoard :: Board -> IO ()
printBoard board = mapM_ (putStrLn . unwords . map (pad . show)) board
  where pad s = replicate (3 - length s) ' ' ++ s

-- Funzione per leggere una posizione dall'input
readPosition :: IO (Int, Int)
readPosition = do
    input <- getLine
    case reads input of
        [(pos, "")] -> return pos
        _ -> do
            putStrLn "Input non valido. Inserisci di nuovo (esempio: (3,3)):"
            readPosition

main :: IO ()
main = do
    putStrLn "Inserisci la dimensione della scacchiera:"
    sizeStr <- getLine
    let size = read sizeStr :: Int
    putStrLn "Inserisci la posizione di partenza del cavaliere (esempio: (3,3)):"
    start <- readPosition
    putStrLn "Attendere la soluzione (timeout di 2 minuti)..."
    
    ref <- newIORef (size * size, replicate size (replicate size (-1)))

    _ <- forkIO $ printStatus ref

    result <- timeout (10 * 60 * 10^6) $ do
        if not (isValidPosition size start)
        then putStrLn "Posizione di partenza non valida." >> return Nothing
        else do
            let board = replicate size (replicate size (-1))
                initialBoard = updateBoard board start 0
            case size of
                3 -> backtracking size initialBoard start 1 ref
                4 -> backtracking size initialBoard start 1 ref
                5 -> backtracking size initialBoard start 1 ref
                6 -> warnsdorff size initialBoard start 1 ref
                7 -> warnsdorff size initialBoard start 1 ref
                8 -> warnsdorff size initialBoard start 1 ref
                _ -> backtrackingWithWarnsdorff size initialBoard start 1 ref
    case result of
        Just (Just solution) -> do
            clearScreen
            printBoard solution
        Just Nothing -> putStrLn "Soluzione non trovata"
        Nothing -> putStrLn "Timeout: il programma ha impiegato troppo tempo."

printStatus :: IORef (Int, Board) -> IO ()
printStatus ref = do
    (missing, board) <- readIORef ref
    clearScreen
    putStrLn $ "Caselle mancanti: " ++ show missing
    printBoard board
    threadDelay (5 * 10^6)
    printStatus ref

-- Verifica se la posizione iniziale è valida
isValidPosition :: Int -> Position -> Bool
isValidPosition n (x, y) = x >= 0 && x < n && y >= 0 && y < n
