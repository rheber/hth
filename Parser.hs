module Parser where
import Text.Parsec
import Text.Parsec.Char

import Command
import Time (TimePeriod(..))

type Parser a = Parsec String () a

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
delete = Delete <$> ((string "delete " <|> string "remove ") *> integer)

help :: Parser Command
help = string "help" *> pure Help

list :: Parser Command
list = List <$> ((optional $ string "list ") *> timePeriod)

mark :: Parser Command
mark = Mark <$> (string "mark " *> integer)

monthly :: Parser Command
monthly = Monthly <$> (string "monthly " *> taskName)

quitUnsafe :: Parser Command
quitUnsafe = string "quit!" *> pure QuitUnsafe

quit :: Parser Command
quit = (string "quit" <|> string "exit") *> pure Quit

rename :: Parser Command
rename = Rename <$> (string "rename " *> integer) <* space <*> taskName

renumber :: Parser Command
renumber = string "renumber" *> pure Renumber

retime :: Parser Command
retime = Retime <$> (string "retime " *> integer)

save :: Parser Command
save = string "save" *> pure Save

swap :: Parser Command
swap = Swap <$> (string "swap " *> integer) <* space <*> integer

weekly :: Parser Command
weekly = Weekly <$> (string "weekly " *> taskName)

command :: Parser Command
command =
  try delete <|>
  help <|>
  try mark <|>
  try monthly <|>
  try quitUnsafe <|>
  quit <|> -- "try" would be needed if there were another "e" command.
  try rename <|>
  try renumber <|>
  retime <|>
  try save <|>
  swap <|>
  try weekly <|>
  list -- comes after "weekly" and "monthly"

parseExpr :: String -> Command
parseExpr input =
  case parse command "stdin" input of
    Left err -> ParseFailure
    Right expr -> expr
