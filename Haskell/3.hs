import Data.Time (Day, TimeOfDay(..), fromGregorian, parseTimeM, defaultTimeLocale, formatTime)
import Data.List (find, delete)
import System.IO
import Control.Monad (when)
import Text.Read (readMaybe)
import System.Directory (doesFileExist)

data Appointment = Appointment
  { date :: Day
  , time :: TimeOfDay
  , description :: String
  , participants :: [String]
  , recurring :: Maybe Recurrence
  } deriving (Show, Eq, Read)

data Recurrence = Daily | Weekly | Monthly deriving (Show, Eq, Read)

-- Aggiungere un nuovo appuntamento
addAppointment :: [Appointment] -> Appointment -> [Appointment]
addAppointment appointments newAppointment = newAppointment : appointments

-- Eliminare un appuntamento esistente
removeAppointment :: [Appointment] -> Appointment -> [Appointment]
removeAppointment appointments appointmentToRemove = delete appointmentToRemove appointments

-- Verificare conflitti di orario
hasConflict :: Appointment -> Appointment -> Bool
hasConflict a1 a2 = date a1 == date a2 && time a1 == time a2

checkConflicts :: [Appointment] -> Appointment -> Bool
checkConflicts appointments newAppointment = any (hasConflict newAppointment) appointments

-- Elenco degli appuntamenti per una data specificata
appointmentsForDate :: [Appointment] -> Day -> [Appointment]
appointmentsForDate appointments searchDate = filter (\appt -> date appt == searchDate) appointments

-- Ricerca di appuntamenti per descrizione
searchByDescription :: [Appointment] -> String -> [Appointment]
searchByDescription appointments searchTerm = filter (\appt -> description appt == searchTerm) appointments

-- Ricerca di appuntamenti per partecipante
searchByParticipant :: [Appointment] -> String -> [Appointment]
searchByParticipant appointments participant = filter (\appt -> participant `elem` participants appt) appointments

-- Aggiornare un appuntamento esistente
updateAppointment :: [Appointment] -> Appointment -> Appointment -> [Appointment]
updateAppointment appointments oldAppointment newAppointment =
  newAppointment : delete oldAppointment appointments

-- Salvare gli appuntamenti in un file
saveAppointments :: FilePath -> [Appointment] -> IO ()
saveAppointments filePath appointments = writeFile filePath (show appointments)

-- Caricare gli appuntamenti da un file
loadAppointments :: FilePath -> IO [Appointment]
loadAppointments filePath = do
  fileExists <- doesFileExist filePath
  if fileExists
    then do
      contents <- readFile filePath
      return (read contents :: [Appointment])
    else return []

-- Funzione principale
main :: IO ()
main = do
  let filePath = "appointments.txt"
  initialAppointments <- loadAppointments filePath
  putStrLn "Gestione Calendario"
  menu initialAppointments filePath

menu :: [Appointment] -> FilePath -> IO ()
menu appointments filePath = do
  putStrLn "1. Aggiungere un nuovo appuntamento"
  putStrLn "2. Eliminare un appuntamento esistente"
  putStrLn "3. Verificare conflitti di orario"
  putStrLn "4. Elenco degli appuntamenti per una data specificata"
  putStrLn "5. Ricerca di appuntamenti per descrizione"
  putStrLn "6. Ricerca di appuntamenti per partecipante"
  putStrLn "7. Aggiornare un appuntamento esistente"
  putStrLn "8. Salvare gli appuntamenti"
  putStrLn "9. Caricare gli appuntamenti"
  putStrLn "10. Esci"
  choice <- getLine
  case choice of
    "1" -> do
      newAppointment <- getAppointmentFromUser
      let newAppointments = addAppointment appointments newAppointment
      menu newAppointments filePath
    "2" -> do
      putStrLn "Inserisci la descrizione dell'appuntamento da eliminare:"
      desc <- getLine
      let maybeAppointment = find (\appt -> description appt == desc) appointments
      case maybeAppointment of
        Just appt -> do
          let newAppointments = removeAppointment appointments appt
          menu newAppointments filePath
        Nothing -> do
          putStrLn "Appuntamento non trovato."
          menu appointments filePath
    "3" -> do
      newAppointment <- getAppointmentFromUser
      let conflict = checkConflicts appointments newAppointment
      if conflict
        then putStrLn "Conflitto trovato."
        else putStrLn "Nessun conflitto."
      menu appointments filePath
    "4" -> do
      putStrLn "Inserisci la data (AAAA-MM-GG):"
      dateString <- getLine
      let maybeDate = parseTimeM True defaultTimeLocale "%Y-%m-%d" dateString :: Maybe Day
      case maybeDate of
        Just d -> do
          let results = appointmentsForDate appointments d
          print results
        Nothing -> putStrLn "Data non valida."
      menu appointments filePath
    "5" -> do
      putStrLn "Inserisci la descrizione:"
      desc <- getLine
      let results = searchByDescription appointments desc
      print results
      menu appointments filePath
    "6" -> do
      putStrLn "Inserisci il nome del partecipante:"
      participant <- getLine
      let results = searchByParticipant appointments participant
      print results
      menu appointments filePath
    "7" -> do
      putStrLn "Inserisci la descrizione dell'appuntamento da aggiornare:"
      desc <- getLine
      let maybeAppointment = find (\appt -> description appt == desc) appointments
      case maybeAppointment of
        Just oldAppointment -> do
          putStrLn "Inserisci i nuovi dettagli dell'appuntamento:"
          newAppointment <- getAppointmentFromUser
          let newAppointments = updateAppointment appointments oldAppointment newAppointment
          menu newAppointments filePath
        Nothing -> do
          putStrLn "Appuntamento non trovato."
          menu appointments filePath
    "8" -> do
      saveAppointments filePath appointments
      putStrLn "Appuntamenti salvati."
      menu appointments filePath
    "9" -> do
      newAppointments <- loadAppointments filePath
      putStrLn "Appuntamenti caricati."
      menu newAppointments filePath
    "10" -> putStrLn "Arrivederci!"
    _ -> do
      putStrLn "Scelta non valida."
      menu appointments filePath

getAppointmentFromUser :: IO Appointment
getAppointmentFromUser = do
  putStrLn "Inserisci la data (AAAA-MM-GG):"
  dateString <- getLine
  let maybeDate = parseTimeM True defaultTimeLocale "%Y-%m-%d" dateString :: Maybe Day
  putStrLn "Inserisci l'ora (HH:MM):"
  timeString <- getLine
  let maybeTime = parseTimeM True defaultTimeLocale "%H:%M" timeString :: Maybe TimeOfDay
  putStrLn "Inserisci la descrizione:"
  desc <- getLine
  putStrLn "Inserisci i partecipanti (separati da virgola):"
  participantsString <- getLine
  let participants = wordsWhen (== ',') participantsString
  putStrLn "L'appuntamento è ricorrente? (nessuno, giornaliero, settimanale, mensile):"
  recString <- getLine
  let recurrence = case recString of
                     "giornaliero" -> Just Daily
                     "settimanale" -> Just Weekly
                     "mensile" -> Just Monthly
                     _ -> Nothing
  case (maybeDate, maybeTime) of
    (Just d, Just t) -> return $ Appointment d t desc participants recurrence
    _ -> do
      putStrLn "Data o ora non valida."
      getAppointmentFromUser

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where (w, s'') = break p s'
