module Time where
import Data.Time

data TimePeriod = AnyTime | Week | Month deriving (Eq, Read, Show)

dummyTime = UTCTime (ModifiedJulianDay 0) $ secondsToDiffTime 0

secsPerDay = 24 * 60 * 60

atCurrentTime :: (UTCTime -> IO a) -> IO a
atCurrentTime f = getCurrentTime >>= f

timeAfter :: TimePeriod -> UTCTime -> UTCTime
timeAfter Week = addUTCTime (7 * secsPerDay)
timeAfter Month = addUTCTime (30 * secsPerDay)
