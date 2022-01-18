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
