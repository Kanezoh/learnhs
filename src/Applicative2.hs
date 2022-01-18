val1 :: Maybe String
val1 = Just "hoge"

val2 :: Maybe String
val2 = Just "hoge2"

concatVal :: Maybe String
concatVal = (++) <$> val1 <*> val2

maybeInt :: Maybe(Int -> Int)
maybeInt = pure (6+)

helloIO :: Maybe String
helloIO = pure "Hello, World"

-- 素数生成アルゴリズム
primesToN :: Integer -> [Integer]
primesToN n = filter isNotComposite twoThroughN
  where twoThroughN = [2 .. n]
        composite = pure (*) <*> twoThroughN <*> twoThroughN -- 合成数
        isNotComposite = not . (`elem` composite)

-- 大量のテストデータを作成する
data User = User { name :: String 
                 , gamerId :: Int
                 , score :: Int } deriving Show

testNames :: [String]
testNames = [ "John"
            , "Smith"
            , "Paul"
            , "Sara"]

testIds :: [Int]
testIds = [111,112,113]

testScores :: [Int]
testScores = [30221, 2141234, 142124,412414,412414]

testUsers :: [User]
testUsers = pure (User) <*> testNames <*> testIds <*> testScores

