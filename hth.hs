import Control.Exception hiding (try)
import Control.Monad (foldM)
import qualified Data.Map.Lazy as Map
import Data.Time
import GHC.IO.Handle (hFlush)
import System.Exit (exitSuccess)
import System.IO (stdout)
import Text.Parsec
import Text.Parsec.Char

type Parser a = Parsec String () a
data TimePeriod = AnyTime | Week | Month deriving (Eq, Read, Show)
data Task = Task String TimePeriod UTCTime deriving (Read, Show)

-- Next task ID to be assigned, whether unsaved changes exist, the tasks.
data HTHState = HTHState {
  counter :: Int,
  isModified :: Bool,
  taskMap ::Map.Map Int Task
} deriving Show

data Command =
  Delete Int |
  Help |
  List TimePeriod |
  Mark Int |
  Monthly String |
  QuitUnsafe |
  Quit |
  ParseFailure |
  Rename Int String |
  Renumber |
  Retime Int |
  Save |
  Weekly String
  deriving Show

{- Time functions. -}

secsPerDay = 24 * 60 * 60

timeAfter :: TimePeriod -> UTCTime -> UTCTime
timeAfter Week = addUTCTime (7 * secsPerDay)
timeAfter Month = addUTCTime (30 * secsPerDay)

-- Whether a task is overdue.
isUrgent :: UTCTime -> Task -> Bool
isUrgent now (Task _ p t) = timeAfter p t < now

{- Parsers for basic types. -}

integer :: Parser Int
integer = fmap read $ many1 digit

taskName :: Parser String
taskName = many1 anyChar

timePeriod :: Parser TimePeriod
timePeriod =
  string "all" *> pure AnyTime <|>
  string "monthly" *> pure Month <|>
  string "weekly" *> pure Week

{- Parsers for commands. -}

delete :: Parser Command
delete = Delete <$> (string "delete " *> integer)

help :: Parser Command
help = string "help" *> pure Help

list :: Parser Command
list = List <$> (string "list " *> timePeriod)

mark :: Parser Command
mark = Mark <$> (string "mark " *> integer)

monthly :: Parser Command
monthly = Monthly <$> (string "monthly " *> taskName)

quitUnsafe :: Parser Command
quitUnsafe = string "quit!" *> pure QuitUnsafe

quit :: Parser Command
quit = string "quit" *> pure Quit

rename :: Parser Command
rename = Rename <$> (string "rename " *> integer) <* space <*> taskName

renumber :: Parser Command
renumber = string "renumber" *> pure Renumber

retime :: Parser Command
retime = Retime <$> (string "retime " *> integer)

save :: Parser Command
save = string "save" *> pure Save

weekly :: Parser Command
weekly = Weekly <$> (string "weekly " *> taskName)

command :: Parser Command
command =
  delete <|>
  help <|>
  list <|>
  try mark <|>
  monthly <|>
  try quitUnsafe <|>
  quit <|>
  try rename <|>
  try renumber <|>
  retime <|>
  save <|>
  weekly

{- Functions which implement commands. -}

addTask :: HTHState -> Task -> IO HTHState
addTask (HTHState i _ m) t = return $ HTHState (i + 1) True $ Map.insert i t m

deleteTask :: HTHState -> Int -> IO HTHState
deleteTask (HTHState i _ m) n = return $ HTHState i True $ Map.delete n m

listTask :: Int -> Task -> IO ()
listTask n (Task name p deadline) = do
  now <- getCurrentTime
  putStrLn $ show n ++ " [" ++
    (if now < deadline then "X" else " ") ++
    "] " ++ name

listTasks :: HTHState -> TimePeriod -> IO HTHState
listTasks st@(HTHState _ _ m) AnyTime = Map.traverseWithKey listTask m >> return st
listTasks st@(HTHState _ _ m) p =
  Map.traverseWithKey listTask (Map.filter (\(Task _ q  _) -> q == p) m) >>
  return st

markTask :: HTHState -> Int -> IO HTHState
markTask (HTHState i _ m) n = let
  modify (Task s p t) = Task s p $ timeAfter p t
  in return $ HTHState i True $ Map.adjust modify n m

quitSafe :: HTHState -> IO HTHState
quitSafe st =
  if isModified st
  then putStrLn "Unsaved changes, either 'save' or 'quit!'" >> return st
  else exitSuccess

saveTasks :: HTHState -> IO HTHState
saveTasks st =
  writeFile "habits" (concatMap (++ "\n") $ show <$> taskMap st) >>
  return st{isModified = False}

{- Setup and repl functions. -}

parseExpr :: String -> Command
parseExpr input =
  case parse command "stdin" input of
    Left err -> ParseFailure
    Right expr -> expr

evalExpr :: HTHState -> Command -> IO HTHState
evalExpr st input = do
  now <- getCurrentTime
  case input of
    Delete n -> deleteTask st n
    List p -> listTasks st p
    Mark n -> markTask st n
    Monthly name -> addTask st $ Task name Month now
    ParseFailure -> putStrLn "Unrecgonised/incomplete command" >> return st
    QuitUnsafe -> exitSuccess
    Quit -> quitSafe st
    Save -> saveTasks st
    Weekly name -> addTask st $ Task name Week now
    _ -> putStrLn "Unimplemented" >> return st

announce :: Task -> IO Task
announce task@(Task name _ _) = do
  now <- getCurrentTime
  if isUrgent now task then putStrLn ("Outstanding task: " ++ name) else return ()
  return task

loadTasks :: HTHState -> IO HTHState
loadTasks st = do
  taskString <- catch (readFile "habits") (\(SomeException _) -> return "")
  taskStrings <- sequence $ announce <$> read <$> lines taskString
  st <- foldM addTask st taskStrings
  return st{isModified = False}

setup :: IO HTHState
setup = do
  let tasks = Map.empty
  loadTasks $ HTHState 1 True tasks

repl :: HTHState -> IO HTHState
repl st =
  putStr "hth> " >>
  hFlush stdout >>
  getLine >>=
  \input -> (evalExpr st $ parseExpr input) >>=
  \newState -> repl newState

main :: IO HTHState
main = setup >>= \s -> repl s
