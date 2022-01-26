echo :: IO()
-- echo = getLine >>= putStrLn

echo = do
  name <- getLine
  putStrLn name

main :: IO()
main = echo
