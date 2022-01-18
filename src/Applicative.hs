import qualified Data.Map as Map

type LatLong = (Double, Double)

locationDB :: Map.Map String LatLong
locationDB = Map.fromList [ ("Arkham", (42.6054,-70.7829))
                          , ("Innsmouth", (42.8250,-70.8150))
                          , ("Carcosa", (29.9714,-90.7694))
                          , ("New York", (40.7776,-73.9691))]
-- ラジアンに変換
toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

-- 緯度経度をラジアンに変換
latLongToRads :: LatLong -> (Double, Double)
latLongToRads (lat,long) = (rlat, rlong)
  where rlat = toRadians lat
        rlong = toRadians long

-- 2点間の距離を求める
haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 = earthRadius * c
  where (rlat1, rlong1) = latLongToRads coords1
        (rlat2, rlong2) = latLongToRads coords2
        dlat = rlat2 - rlat1
        dlong = rlong2 - rlong1
        a = (sin (dlat/2))^2 + cos rlat1 * cos rlat2 * (sin (dlong/2))^2
        c = 2 * atan2 (sqrt a) (sqrt (1-a))
        earthRadius = 3961.0

printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn "Error, invalid city site"
printDistance (Just distance) = putStrLn (show distance ++ " miles")


-- Applicative <*>

incMaybe :: Maybe(Int -> Int)
incMaybe = (+) <$> Just 1

-- <*>で繋げる
-- incMaybe <*> Just 1

concatMaybe :: Maybe(String -> String)
concatMaybe = (++) <$> Just "hoge"

appMain :: IO()
appMain = do
  putStrLn "Enter the starting city name:"
  startingInput <- getLine
  let startingCity = Map.lookup startingInput locationDB
  putStrLn "Enter the destination city name:"
  destInput <- getLine
  let destCity = Map.lookup destInput locationDB
  let distance = haversine <$> startingCity <*> destCity
  printDistance distance
