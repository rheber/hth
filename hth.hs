import GHC.IO.Handle (hFlush)
import System.IO (stdout)

import Parser (parseExpr)
import State (HTHState(..), configureState, evalExpr, loadTasks, emptyState)
import Time (atCurrentTime)

setupState :: IO HTHState
setupState = loadTasks =<< configureState emptyState

repl :: HTHState -> IO HTHState
repl st =
  putStr "hth> " >>
  hFlush stdout >>
  getLine >>=
  \input -> atCurrentTime (evalExpr st $ parseExpr input) >>=
  \newState -> repl newState

main :: IO HTHState
main = setupState >>= repl
