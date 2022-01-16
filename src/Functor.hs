import qualified Data.Map as Map

halve :: Int -> Double
halve n = fromIntegral n / 2.0

halveMaybe :: Maybe Int -> Maybe Double
halveMaybe (Just n) = Just (halve n)
halveMaybe Nothing = Nothing

successfulRequest :: Maybe Int
successfulRequest = Just 6

failedRequest :: Maybe Int
failedRequest = Nothing

incMaybe :: Maybe Int -> Maybe Int
incMaybe (Just n) = Just (n + 1)
incMaybe Nothing = Nothing

reverseMaybe :: Maybe String -> Maybe String
reverseMaybe (Just str) = Just (reverse str)
reverseMaybe Nothing = Nothing

-- 実際の定義
--instance Functor Maybe where
--  fmap func (Just n) = Just (func n)
--  fmap func Nothing = Nothing

successStr :: Maybe String
successStr = show <$> successfulRequest

failStr :: Maybe String
failStr = show <$> failedRequest

data RobotPart = RobotPart { name :: String
                           , description :: String
                           , cost :: Double
                           , count :: Int } deriving Show

leftArm :: RobotPart
leftArm = RobotPart { name = "left arm"
                    , description = "left arm for face punching!"
                    , cost = 1000.00
                    , count = 3 }
rightArm :: RobotPart
rightArm = RobotPart { name = "right arm"
                     , description = "right arm for face punching!"
                     , cost = 1025.00
                     , count = 5 }
robotHead :: RobotPart
robotHead = RobotPart { name = "robot head"
                      , description = "this head looks mad"
                      , cost = 5092.25
                      , count = 2 }

type HTML = String

renderHTML :: RobotPart -> String
renderHTML part = mconcat [ "<h2>", partName, "</h2>"
                          , "<p><h3>desc</h3>", partDesc
                          , "</p><p><h3>cost</h3>"
                          , partCost
                          , "</p><p><h3>count</h3>"
                          , partCount, "</p>"
                          ]
  where partName = name part
        partDesc = description part
        partCost = show $ cost part
        partCount = show $ count part

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
  where keys = [1,2,3]
        vals = [leftArm, rightArm, robotHead]
        keyVals = zip keys vals

partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

partHTML :: Maybe HTML
partHTML = renderHTML <$> partVal

allParts :: [RobotPart]
allParts = snd <$> (Map.toList partsDB)

allPartsHTML :: [HTML]
allPartsHTML = renderHTML <$> allParts

htmlPartsDB :: Map.Map Int HTML
htmlPartsDB = renderHTML <$> partsDB

data Box a = Box a deriving Show

instance Functor Box where
  fmap func (Box a) = Box (func a)

morePresents :: Int -> Box a -> Box [a]
morePresents n box = (replicate n) <$> box

wrap :: a -> Box a
wrap a = Box a

unwrap :: Box a -> a
unwrap (Box a) = a
