module Command where

import Time (TimePeriod)

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
  Swap Int Int |
  Weekly String
  deriving Show
