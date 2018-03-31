import GHC.IO.Handle (hFlush)
import System.IO (stdout)

import Parser (parseExpr)
import State (HTHState(..), IOS(..),
  configureState, evalExpr, loadTasks, emptyState)
import Time (atCurrentTime)

setupState :: IO HTHState
setupState = loadTasks =<< configureState emptyState

repl :: IOS HTHState
repl st =
  putStr "hth> " >>
  hFlush stdout >>
  getLine >>=
  \input -> atCurrentTime (\t -> evalExpr (parseExpr input) t st) >>=
  \newState -> repl newState

main :: IO HTHState
main = setupState >>= repl
