import Text.Parsec
import Text.Parsec.Char

type Parser a = Parsec String () a
data TimePeriod = AnyTime | Week | Month deriving Show

data Command =
  Delete Int |
  Help |
  List TimePeriod |
  Mark Int |
  Monthly String |
  QuitUnsafe |
  Quit |
  Rename Int String |
  Renumber |
  Retime Int |
  Save |
  Weekly String
  deriving Show

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

parseExpr :: String -> Command
parseExpr input =
  case parse command "stdin" input of
    Left err -> error "Unrecgonised/incomplete command"
    Right expr -> expr

main :: IO ()
main = return ()