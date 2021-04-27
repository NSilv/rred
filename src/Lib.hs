{-# LANGUAGE OverloadedStrings, TemplateHaskell, DerivingStrategies, DeriveAnyClass, DeriveGeneric #-}
module Lib where


import Control.Lens
import qualified Data.ByteString.Lazy as B
import Data.Csv
import qualified Data.Text as T
import Network.Google
import Network.Google.Sheets
import System.Environment
import System.IO
import Control.Applicative
import Control.Lens.TH
import Data.List
import Data.Maybe
import Data.List.Split
import Data.Aeson
import GHC.Generics
spid :: T.Text 
spid = "1TCj8FCZhgfkPW6Qr0CkWkzMeO0vfMrjmTKRSVOpdXTE"

getSheet :: T.Text -> T.Text -> IO [[T.Text]]
getSheet spreadsheetId sheetName = do
  let range = sheetName -- ++"!A1:J10"

  lgr <- newLogger Info stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ spreadsheetsReadOnlyScope)
  values <- view vrValues <$> (runResourceT . runGoogle env $ send  (spreadsheetsValuesGet spreadsheetId range))
  pure $ map (map $ T.pack . drop 8 . init . show) values

d = getGrass spid

getGrass spid = 
  fullTransform 
  . over _2 head 
  . splitRows 
  . map parseRow 
  <$> getSheet spid "Grass"
  
  




data EncTime = Day | Night deriving (Eq, Ord, Show, Read) 

data Cell 
  = EncPercent Int 
  | Mon T.Text
  deriving (Eq,Ord,Show)

data RouteHead 
  = PokePercent
  | RouteName T.Text
  deriving (Eq, Ord, Show)

data TransposeHelper a
  = R RouteHead 
  | C a
  deriving (Show, Eq, Ord)
data Row
  = EncTime EncTime 
  | RouteHeader [RouteHead]
  | Cells [Cell]
  deriving (Eq, Ord, Show)




timeFromRow :: [T.Text] -> Maybe EncTime
timeFromRow [] = Nothing 
timeFromRow xs = 
  readT <$> find (liftA2 (||) (=="Day") (=="Night")) xs

parseRow :: [T.Text] -> Row 
parseRow xs = 
  fromMaybe cells $ maybe routeHead (Just . EncTime) $ timeFromRow xs  
  where
    routeHead = 
      if head xs == "Pokemon %" then
        Just $ RouteHeader $ PokePercent:map RouteName (tail xs)
      else 
        Nothing
    cells = 
      Cells $ map toCell xs 
    toCell s = 
      if hasT '%' s then 
        EncPercent $ readT $ T.replace "%" "" s
      else 
        Mon s

data Route = 
  Route { _routeName :: T.Text, _routePks :: Pks }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)
    
data Pks = 
  Pks { _day :: [Pk], _night :: [Pk] }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Pk = 
  Pk { _pkName :: T.Text, _pkPercent :: Int }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)
tt1 = head <$> d

hasT char txt = 
  isJust $ T.find (==char) txt

readT :: Read a => T.Text -> a
readT = 
  read . T.unpack

splitRows rows =
  (dayRows, nightRows)
  where 
    isTime (EncTime _) = True 
    isTime _ = False
    (day:dayRows:night:nightRows) =
      filter (not . null) $ split (whenElt isTime) rows
 
helper (RouteHeader r) = map R r 
helper (Cells c) = map C c
  

 

attachPercent :: [[TransposeHelper Cell]] -> [[TransposeHelper (Cell, Int)]]
attachPercent (perc:rows) = 
  map (\r -> zipWith go r perc) rows 
  where
    go (R r) _ = R r
    go (C c) (C (EncPercent p)) = C (c,p)
--    go p c = Left $ "unexpected: lhs = " <> show p <> ", rhs = " <> show c


mkRoute (R (RouteName name)) pks = Route name pks 
mkRoute r _ = error $ "[mkRoute] unexpected: " <> show r

mkPk (C (Mon name, perc)) = Pk name perc 
mkPk s = error $ "[mkPk] unexpected: " <> show s

doDay = attachPercent . transpose . map helper



doRoute (d:ds) (n:ns) | d == n =
  mkRoute d pks 
  where
    pks = Pks (map mkPk ds) (map mkPk ns)
doRoute a b =   
  error $ show (a,b)
fullTransform = 
  map (uncurry doRoute) . uncurry zip . over both doDay

