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
data HTHState = HTHState Int (Map.Map Int Task)

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

secsPerDay = 1000 * 24 * 60

timeAfter :: TimePeriod -> UTCTime -> UTCTime
timeAfter Week = addUTCTime (7 * secsPerDay)
timeAfter Month = addUTCTime (30 * secsPerDay)

integer :: Parser Int
integer = fmap read $ many1 digit

taskName :: Parser String
taskName = many1 anyChar

timePeriod :: Parser TimePeriod
timePeriod =
  string "all" *> pure AnyTime <|>
  string "monthly" *> pure Month <|>
  string "weekly" *> pure Week

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

addTask :: HTHState -> Task -> IO HTHState
addTask (HTHState n m) task =
  return $ HTHState (n + 1) $ Map.insert n task m

listTask :: Int -> Task -> IO ()
listTask n (Task name p deadline) = do
  now <- getCurrentTime
  putStrLn $ show n ++ " [" ++
    (if now < deadline then "X" else " ") ++
    "] " ++ name

listTasks :: HTHState -> TimePeriod -> IO HTHState
listTasks st@(HTHState _ m) AnyTime = Map.traverseWithKey listTask m >> return st
listTasks st@(HTHState _ m) p =
  Map.traverseWithKey listTask (Map.filter (\(Task _ q  _) -> q == p) m) >>
  return st

markTask :: HTHState -> Int -> IO HTHState
markTask (HTHState i m) n = let
  modify (Task s p t) = Task s p $ timeAfter p t
  in return $ HTHState i $ Map.adjust modify n m

saveTasks :: HTHState -> IO HTHState
saveTasks st@(HTHState _ m) =
  writeFile "habits" (concatMap (++ "\n") $ show <$> m) >>
  return st

parseExpr :: String -> Command
parseExpr input =
  case parse command "stdin" input of
    Left err -> ParseFailure
    Right expr -> expr

evalExpr :: HTHState -> Command -> IO HTHState
evalExpr st input = do
  now <- getCurrentTime
  case input of
    List p -> listTasks st p
    Mark n -> markTask st n
    Monthly name -> addTask st $ Task name Month now
    ParseFailure -> putStrLn "Unrecgonised/incomplete command" >> return st
    QuitUnsafe -> exitSuccess
    Save -> saveTasks st
    Weekly name -> addTask st $ Task name Week now
    _ -> putStrLn "Unimplemented" >> return st

keepLoading :: HTHState -> [Task] -> IO HTHState
keepLoading st [] = return st
keepLoading st (t:ts) = addTask st t >>= \s -> keepLoading st ts

loadTasks :: HTHState -> IO HTHState
loadTasks st = do
  tasks <- readFile "habits"
  keepLoading st $ read <$> lines tasks

setup :: IO HTHState
setup = do
  let tasks = Map.empty
  loadTasks $ HTHState 1 tasks

repl :: HTHState -> IO HTHState
repl st =
  putStr "hth> " >>
  hFlush stdout >>
  getLine >>=
  \input -> (evalExpr st $ parseExpr input) >>=
  \newState -> repl newState

main :: IO HTHState
main = setup >>= \s -> repl s
