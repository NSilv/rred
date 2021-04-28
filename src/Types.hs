{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
module Types where 
import Control.Lens.TH
import Data.Aeson
import GHC.Generics
import qualified Data.Text as T
data EncTime = Day | Night deriving (Eq, Ord, Show, Read)

data Cell
  = EncPercent Int
  | Mon T.Text
  deriving (Eq, Ord, Show)

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


data Pk = Pk {_pkName :: T.Text, _pkPercent :: Int}
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Pks = Pks {_day :: [Pk], _night :: [Pk]}
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Route = Route {_routeName :: T.Text, _routePks :: Pks}
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

makeLenses ''Pk
makeLenses ''Pks
makeLenses ''Route
