import Data.List (minimumBy, sortBy)
import Data.Ord (comparing)
import Data.Maybe (listToMaybe)
import Control.Concurrent (threadDelay, forkIO)
import System.IO (hFlush, stdout)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

type Position = (Int, Int)
type Board = [[Int]]

-- Movimento del cavaliere
knightMoves :: [Position]
knightMoves = [(2, 1), (1, 2), (-1, 2), (-2, 1), (-2, -1), (-1, -2), (1, -2), (2, -1)]

-- Controlla se una posizione è valida sulla scacchiera
isValid :: Int -> Board -> Position -> Bool
isValid n board (x, y) = x >= 0 && x < n && y >= 0 && y < n && (board !! x !! y) == -1

-- Calcola l'accessibilità di una casella
accessibility :: Int -> Board -> Position -> Int
accessibility n board (x, y) = length $ filter (isValid n board) [(x + dx, y + dy) | (dx, dy) <- knightMoves]

-- Algoritmo di Warnsdorff combinato con Squirrel
knightTourWarnsdorffSquirrel :: Int -> Position -> IO (Maybe Board)
knightTourWarnsdorffSquirrel n start = do
    ref <- newIORef (n * n, initializeBoard n)
    result <- warnsdorffSquirrel n (initializeBoard n) start 1 ref
    return result

warnsdorffSquirrel :: Int -> Board -> Position -> Int -> IORef (Int, Board) -> IO (Maybe Board)
warnsdorffSquirrel n board pos move ref = do
    let missing = n * n - move
    writeIORef ref (missing, board)
    if move == n * n 
        then return (Just board)
        else do
            let nextMoves = filter (isValid n board) [(fst pos + dx, snd pos + dy) | (dx, dy) <- knightMoves]
            if null nextMoves
                then return Nothing
                else do
                    let sortedMoves = sortMoves n board nextMoves
                    warnsdorffSquirrel n (updateBoard board (snd $ head sortedMoves) move) (snd $ head sortedMoves) (move + 1) ref

-- Ordina le mosse in base all'accessibilità e alla distanza euclidea dal centro
sortMoves :: Int -> Board -> [Position] -> [(Int, Position)]
sortMoves n board moves = sortBy (comparing fst) $ map (\pos -> (accessibility n board pos, pos)) moves

-- Aggiorna la scacchiera con la nuova mossa
updateBoard :: Board -> Position -> Int -> Board
updateBoard board (x, y) move = take x board ++
                                [take y (board !! x) ++ [move] ++ drop (y + 1) (board !! x)] ++
                                drop (x + 1) board

-- Inizializza la scacchiera
initializeBoard :: Int -> Board
initializeBoard n = replicate n (replicate n (-1))

-- Stampa la scacchiera
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
    putStrLn "Inserisci la posizione di partenza del cavaliere (X, Y):"
    start <- readPosition
    putStrLn "Attendere la soluzione..."
    result <- knightTourWarnsdorffSquirrel size start
    case result of
        Just solution -> printBoard solution
        Nothing -> putStrLn "Soluzione non trovata."
