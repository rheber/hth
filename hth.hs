import Text.Parsec
import Text.Parsec.Char

data Command =
  Delete Int |
  Mark Int |
  Monthly String |
  Quit |
  Weekly String
  deriving Show

type Parser a = Parsec String () a

integer :: Parser Int
integer = fmap read $ many1 digit

taskName :: Parser String
taskName = many1 anyChar

delete :: Parser Command
delete = Delete <$> (string "delete " *> integer)

mark :: Parser Command
mark = Mark <$> (string "mark " *> integer)

monthly :: Parser Command
monthly = Monthly <$> (string "monthly " *> taskName)

quit :: Parser Command
quit = do
  string "quit"
  return Quit

weekly :: Parser Command
weekly = Weekly <$> (string "weekly " *> taskName)

command :: Parser Command
command = delete <|>
  try mark <|>
  monthly <|>
  quit <|>
  weekly

parseExpr :: String -> Command
parseExpr input =
  case parse command "stdin" input of
    Left err -> error (show err)
    Right expr -> expr
