import qualified Data.Map as Map

type UserName = String
type GamerId = Int
type PlayerCredits = Int

-- GamerIdに基づいてUserNameを取得するためのDB
userNameDB :: Map.Map GamerId UserName
userNameDB = Map.fromList [ (1, "ahgaskgha")
                           ,(2, "ahgajg")
                           ,(3, "agowgjjagal")
                           ,(4, "lhaghlakgah")
                           ,(5, "l;ukhagbh")
                           ,(6, ";ghajkghaklhb")]

-- UserNameに基づいてPlayerCreditsを取得するためのDB
creditsDB :: Map.Map UserName PlayerCredits
creditsDB = Map.fromList [ ("ahgaskgha", 2000)
                          ,("ahgajg", 15000)
                          ,("agowgjjagal", 300)
                          ,("lhaghlakgah", 12)
                          ,("l;ukhagbh", 50000)
                          ,(";ghajkghaklhb", 150000)]

creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId id = altLookupCredits $ lookupUserName id

lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits username = Map.lookup username creditsDB

altLookupCredits :: Maybe UserName -> Maybe PlayerCredits
altLookupCredits Nothing = Nothing
altLookupCredits (Just username) = lookupCredits username

main :: IO()
main = putStrLn ""
