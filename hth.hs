import GHC.IO.Handle (hFlush)
import System.IO (stdout)

import Parser (parseExpr)
import State (HTHState(..), evalExpr, setupState)
import Time (atCurrentTime)

repl :: HTHState -> IO HTHState
repl st =
  putStr "hth> " >>
  hFlush stdout >>
  getLine >>=
  \input -> atCurrentTime (evalExpr st $ parseExpr input) >>=
  \newState -> repl newState

main :: IO HTHState
main = setupState >>= repl
