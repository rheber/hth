module Task where
import Data.Time (UTCTime)

import Time (TimePeriod(..), dummyTime, timeAfter)

data Task = Task String TimePeriod UTCTime deriving (Read, Show)
dummyTask = Task "" AnyTime dummyTime

-- Whether a task is overdue.
isUrgent :: UTCTime -> Task -> Bool
isUrgent now (Task _ p t) = timeAfter p t < now

announce :: Task -> UTCTime -> IO ()
announce task@(Task name _ _) now =
  if isUrgent now task then putStrLn ("Outstanding task: " ++ name) else return ()

pad :: Int -> String
pad n = if n < 10 then (' ':show n) else show n

listTaskString :: Int -> String -> UTCTime -> UTCTime -> String
listTaskString n name deadline now =
  pad n ++ " [" ++ (if now < deadline then "X" else " ") ++ "] " ++ name

listTask :: UTCTime -> Int -> Task -> IO ()
listTask now n (Task name _ deadline) =
  putStrLn $ listTaskString n name deadline now
