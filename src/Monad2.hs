import qualified Data.Map as Map

echo :: IO()
-- echo = getLine >>= putStrLn

echo = do
  name <- getLine
  putStrLn name

data Grade = F | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)

data Degree = HS | BA | MS | PhD deriving (Eq, Ord, Enum, Show, Read)

data Candidate = Candidate { candidateId :: Int
                           , codeReview :: Grade
                           , cultureFit :: Grade
                           , education :: Degree } deriving Show

-- 候補者が要件を満たしているかどうか
viable :: Candidate -> Bool
viable candidate = all (== True) tests
  where passedCoding = codeReview candidate > B
        passedCultureFit = cultureFit candidate > C
        educationMin = education candidate >= MS
        tests = [passedCoding, passedCultureFit, educationMin]

-- コマンドラインから候補者の情報を受け取るIOアクション
readInt :: IO Int
readInt = getLine >>= (return . read)

-- readGrade = do
--   grade <- getLine
--   return $ read grade
readGrade :: IO Grade
readGrade = getLine >>= (return . read)

readDegree :: IO Degree
readDegree = getLine >>= (return . read)

readCandidate :: IO Candidate
readCandidate = do
  putStrLn "enter id"
  cId <- readInt
  putStrLn "enter code grade"
  codeGrade <- readGrade
  putStrLn "enter culture fit grade"
  cultureFitGrade <- readGrade
  putStrLn "enter education"
  degree <- readDegree
  return (Candidate { candidateId = cId
                    , codeReview = codeGrade
                    , cultureFit = cultureFitGrade
                    , education = degree})

-- Maybeコンテキスト
candidate1 :: Candidate
candidate1 = Candidate { candidateId = 1
                       , codeReview = A
                       , cultureFit = A
                       , education = BA }

candidate2 :: Candidate
candidate2 = Candidate { candidateId = 2
                       , codeReview = C
                       , cultureFit = A
                       , education = PhD }

candidate3 :: Candidate
candidate3 = Candidate { candidateId = 3
                       , codeReview = A
                       , cultureFit = B
                       , education = MS }

candidateDB :: Map.Map Int Candidate
candidateDB = Map.fromList (zip [1,2,3] [candidate1, candidate2, candidate3])

accessCandidateDB :: Int -> Maybe String
accessCandidateDB cId = do
  candidate <- Map.lookup cId candidateDB
  let passed = viable candidate
  let statement = if passed
                  then "passed"
                  else "failed"
  return statement

--test :: Maybe String -> String
--test Nothing = "error id not found"
--test (Just a) = a

candidates :: [Candidate]
candidates = [candidate1, candidate2, candidate3]

accessCandidateList :: [Candidate] -> [String]
accessCandidateList candidates = do
  candidate <- candidates
  let passed = viable candidate
  let statement = if passed
                  then "passed"
                  else "failed"
  return statement

main :: IO()
main = echo
