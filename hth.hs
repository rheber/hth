import Control.Exception (SomeException(..), catch)
import Data.Time (UTCTime)
import qualified Data.Map.Lazy as Map
import GHC.IO.Handle (hFlush)
import System.Exit (exitSuccess)
import System.IO (stdout)

import Command
import Parser (parseExpr)
import Time (TimePeriod(..), dummyTime, atCurrentTime, timeAfter)

data Task = Task String TimePeriod UTCTime deriving (Read, Show)
dummyTask = Task "" AnyTime dummyTime

-- Next task ID to be assigned, whether unsaved changes exist, the tasks.
data HTHState = HTHState {
  counter :: Int,
  isModified :: Bool,
  taskMap :: Map.Map Int Task
} deriving Show

-- Whether a task is overdue.
isUrgent :: UTCTime -> Task -> Bool
isUrgent now (Task _ p t) = timeAfter p t < now

{- Functions which implement commands. -}

addTask :: HTHState -> Task -> HTHState
addTask (HTHState i _ m) t = HTHState (i + 1) True $ Map.insert i t m

deleteTask :: HTHState -> Int -> HTHState
deleteTask (HTHState i _ m) n = HTHState i True $ Map.delete n m

helpPrint :: IO ()
helpPrint = do
  putStrLn ""
  putStrLn "Commands:"
  putStrLn "delete <number>\t\tDelete task"
  putStrLn "help\t\t\tDisplay this message"
  putStrLn "list all/monthly/weekly List tasks in specified category"
  putStrLn "mark <number>\t\tMark task as having been done recently"
  putStrLn "monthly <name>\t\tAdd new monthly task"
  putStrLn "quit\t\t\tQuit if there are no unsaved changes"
  putStrLn "quit!\t\t\tQuit without saving"
  putStrLn "rename <number> <name>\tGive a task a new name"
  putStrLn "renumber\t\tReset task numbering (happens automatically on exit)"
  putStrLn "retime <number>\t\tChange a monthly task to weekly and vice versa"
  putStrLn "save\t\t\tSave any changes"
  putStrLn "swap <number> <number>\tSwap two tasks"
  putStrLn "weekly <name>\t\tAdd new weekly task"
  putStrLn ""

pad :: Int -> String
pad n = if n < 10 then (' ':show n) else show n

listTaskString :: Int -> String -> UTCTime -> UTCTime -> String
listTaskString n name deadline now =
  pad n ++ " [" ++ (if now < deadline then "X" else " ") ++ "] " ++ name

listTask :: UTCTime -> Int -> Task -> IO ()
listTask now n (Task name _ deadline) =
  putStrLn $ listTaskString n name deadline now

listTasks :: HTHState -> TimePeriod -> UTCTime -> IO (Map.Map Int ())
listTasks st@(HTHState _ _ m) AnyTime now = Map.traverseWithKey (listTask now) m
listTasks st@(HTHState _ _ m) p now =
  Map.traverseWithKey (listTask now) (Map.filter (\(Task _ q  _) -> q == p) m)

markTask :: HTHState -> Int -> UTCTime -> HTHState
markTask (HTHState i _ m) n now = let
  modify (Task s p t) = Task s p $ timeAfter p now
  in HTHState i True $ Map.adjust modify n m

quitSafe :: HTHState -> IO HTHState
quitSafe st =
  if isModified st
  then putStrLn "Unsaved changes, either 'save' or 'quit!'" >> return st
  else exitSuccess

renameTask :: HTHState -> Int -> String -> HTHState
renameTask (HTHState i _ m) n name = let
  modify (Task _ p t) = Task name p t
  in HTHState i True $ Map.adjust modify n m

renumberTasks :: HTHState -> HTHState
renumberTasks st = foldl addTask (HTHState 1 True Map.empty) $ taskMap st

retimeTask :: HTHState -> Int -> HTHState
retimeTask (HTHState i _ m) n = let
  modify (Task s Week t) = Task s Month t
  modify (Task s Month t) = Task s Week t
  in HTHState i True $ Map.adjust modify n m

saveTasks :: HTHState -> IO HTHState
saveTasks st =
  writeFile "habits" (concatMap (++ "\n") $ show <$> taskMap st) >>
  return st{isModified = False}

swapTasks :: HTHState -> Int -> Int -> HTHState
swapTasks (HTHState i _ m) a b = let
  aTask = Map.findWithDefault dummyTask a m
  bTask = Map.findWithDefault dummyTask b m
  in HTHState i True $ Map.insert a bTask $ Map.insert b aTask m

{- Setup and repl functions. -}

evalExpr :: HTHState -> Command -> UTCTime -> IO HTHState
evalExpr st input now = do
  case input of
    Delete n -> return $ deleteTask st n
    Help -> helpPrint >> return st
    List p -> (listTasks st p now) >> return st
    Mark n -> return $ markTask st n now
    Monthly name -> return $ addTask st $ Task name Month now
    ParseFailure -> putStrLn "Unrecgonised/incomplete command" >> return st
    QuitUnsafe -> exitSuccess
    Quit -> quitSafe st
    Rename n s -> return $ renameTask st n s
    Renumber -> return $ renumberTasks st
    Retime n -> return $ retimeTask st n
    Save -> saveTasks st
    Swap a b -> return $ swapTasks st a b
    Weekly name -> return $ addTask st $ Task name Week now
--    _ -> putStrLn "Unimplemented" >> return st

announce :: Task -> UTCTime -> IO ()
announce task@(Task name _ _) now =
  if isUrgent now task then putStrLn ("Outstanding task: " ++ name) else return ()

loadTasks :: HTHState -> IO HTHState
loadTasks st = do
  taskString <- catch (readFile "habits") (\(SomeException _) -> return "")
  let taskStrings = read <$> lines taskString
  sequence $ (atCurrentTime . announce) <$> taskStrings
  let new = foldl addTask st taskStrings
  return new{isModified = False}

setup :: IO HTHState
setup = loadTasks $ HTHState 1 True Map.empty

repl :: HTHState -> IO HTHState
repl st =
  putStr "hth> " >>
  hFlush stdout >>
  getLine >>=
  \input -> atCurrentTime (evalExpr st $ parseExpr input) >>=
  \newState -> repl newState

main :: IO HTHState
main = setup >>= repl
