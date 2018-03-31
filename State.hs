module State where

import Control.Exception (SomeException(..), catch)
import qualified Data.Map.Lazy as Map
import Data.Time (UTCTime)
import System.Exit (exitSuccess)
import System.FilePath (combine)

import qualified Command as Cm
import Config (Config(..), defaultConfig, makeConfig)
import Time (TimePeriod(..), atCurrentTime, timeAfter)
import Task (Task(..), announce, dummyTask, listTask)

-- Function which changes its argument with some IO.
type IOS a = a -> IO a

{-
State which includes:
* next task ID to be assigned
* habits file and folder names
* whether unsaved changes exist
* the tasks
-}
data HTHState = HTHState {
  counter :: Int,
  config :: Config,
  isModified :: Bool,
  taskMap :: Map.Map Int Task
} deriving Show
emptyState = HTHState 1 defaultConfig True Map.empty

-- Add config to state.
configureState :: IOS HTHState
configureState st = makeConfig >>= \cfg -> return st{config = cfg}

configFolder = habitsFolder . config
configFilename = habitsFilename . config

habitsPath :: HTHState -> String
habitsPath st = combine (configFolder st) $ configFilename st

-- Low-level HTHState manipulation.

counterInc :: HTHState -> HTHState
counterInc st = st{counter = 1 + counter st}

markModified :: HTHState -> HTHState
markModified st = st{isModified = True}

taskMapInsert :: Int -> Task -> HTHState -> HTHState
taskMapInsert i t st = st{taskMap = Map.insert i t $ taskMap st}

taskMapDelete :: Int -> HTHState -> HTHState
taskMapDelete i st = st{taskMap = Map.delete i $ taskMap st}

taskMapAdjust :: (Task -> Task) -> Int -> HTHState -> HTHState
taskMapAdjust f i st = st{taskMap = Map.adjust f i $ taskMap st}

addTask :: HTHState -> Task -> HTHState
addTask st t = markModified $ counterInc $ taskMapInsert (counter st) t st

deleteTask :: HTHState -> Int -> HTHState
deleteTask st i = markModified $ taskMapDelete i st

-- High-level HTHState manipulation.

listTasks :: HTHState -> TimePeriod -> UTCTime -> IO (Map.Map Int ())
listTasks st AnyTime now = Map.traverseWithKey (listTask now) $ taskMap st
listTasks st p now =
  Map.traverseWithKey (listTask now) $
    Map.filter (\(Task _ q  _) -> q == p) $ taskMap st

markTask :: HTHState -> Int -> UTCTime -> HTHState
markTask st i now = let
  modify (Task s p t) = Task s p $ timeAfter p now
  in markModified $ taskMapAdjust modify i st

quitSafe :: IOS HTHState
quitSafe st =
  if isModified st
  then putStrLn "Unsaved changes, either 'save' or 'quit!'" >> return st
  else exitSuccess

renameTask :: HTHState -> Int -> String -> HTHState
renameTask st i name = let
  modify (Task _ p t) = Task name p t
  in markModified $ taskMapAdjust modify i st

renumberTasks :: HTHState -> HTHState
renumberTasks st = foldl addTask (copyConfig emptyState st) $ taskMap st

retimeTask :: HTHState -> Int -> HTHState
retimeTask st i = let
  modify (Task s Week t) = Task s Month t
  modify (Task s Month t) = Task s Week t
  in markModified $ taskMapAdjust modify i st

saveTasks :: IOS HTHState
saveTasks st =
  writeFile (habitsPath st) (concatMap (++ "\n") $ show <$> taskMap st) >>
  return st{isModified = False}

swapTasks :: HTHState -> Int -> Int -> HTHState
swapTasks st a b = let
  aTask = Map.findWithDefault dummyTask a $ taskMap st
  bTask = Map.findWithDefault dummyTask b $ taskMap st
  in markModified $ taskMapInsert a bTask $ taskMapInsert b aTask st

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

evalExpr :: Cm.Command -> UTCTime -> IOS HTHState
evalExpr input now st =
  case input of
    Cm.Delete n -> return $ deleteTask st n
    Cm.Help -> helpPrint >> return st
    Cm.List p -> (listTasks st p now) >> return st
    Cm.Mark n -> return $ markTask st n now
    Cm.Monthly name -> return $ addTask st $ Task name Month now
    Cm.ParseFailure -> putStrLn "Unrecgonised/incomplete command" >> return st
    Cm.QuitUnsafe -> exitSuccess
    Cm.Quit -> quitSafe st
    Cm.Rename n s -> return $ renameTask st n s
    Cm.Renumber -> return $ renumberTasks st
    Cm.Retime n -> return $ retimeTask st n
    Cm.Save -> saveTasks st
    Cm.Swap a b -> return $ swapTasks st a b
    Cm.Weekly name -> return $ addTask st $ Task name Week now
--    _ -> putStrLn "Unimplemented" >> return st

copyConfig :: HTHState -> HTHState -> HTHState
copyConfig st cfg = st{config = config cfg}

loadTasks :: IOS HTHState
loadTasks st = do
  taskString <- catch (readFile $ habitsPath st) $
    \(SomeException _) -> return ""
  let taskStrings = read <$> lines taskString
  sequence $ (atCurrentTime . announce) <$> taskStrings
  let new = foldl addTask st taskStrings
  return new{isModified = False}
