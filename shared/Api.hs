{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Api where

import Elm (ElmType)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data Request
   = Identify String           -- ^ Set your name before submitting planograms. (Response: none)
   | GetProducts               -- ^ @ AllProducts products @
   | GetReports                -- ^ @ Reports @
   | GetStore String           -- ^ @ GetStore i --> StoreShelfSpace i widths @
   | SubmitPlanogram Planogram -- ^ @ Reports [r] @
   | InternalRefresh           -- ^ Most likely you do not need this. (Refreshes internal server data.)
   deriving (Eq, Show, Read, Generic, ElmType)

data Response
   = AllProducts [Product]
   | StoreShelfSpace StoreId [Width] -- ^ Shelves' widths indexed from 0.
   | Reports [Report]
   | ErrorMsg String
   deriving (Eq, Show, Read, Generic, ElmType)

type Attribute = Int
type StoreId   = String
type ProductId = Int
type ShelfId   = Int
type Width     = Int

data Product = Product
   { ident      :: ProductId -- Unique product identifier
   , width      :: Int
   , priceBuy   :: Double
   , priceSell  :: Double -- note: profit (per unit) equals pPriceSell - pPriceBuy.
   , attributes :: [Attribute] -- ^ @ all (`elem` [1..10]) @
   } deriving (Eq, Show, Read, Generic, ElmType)

data Planogram = Planogram
   { planogramStore        :: StoreId
   , productShelfLocations :: [ProductShelfLocation]
   } deriving (Eq, Show, Read, Generic, ElmType)

data ProductShelfLocation = ProductShelfLocation
   { productId       :: ProductId
   , shelfId         :: ShelfId
   , locationOnShelf :: LocationOnShelf
   } deriving (Eq, Show, Read, Generic, ElmType)

data LocationOnShelf = Top | Middle | Bottom
   deriving (Eq, Show, Read, Generic, ElmType)

-- | A Planogram with items sold and profit made.
data Report = Report
   { planogramUser :: String    -- ^ User identifier.
   , planogram     :: Planogram
   , profit        :: Double
   , productsSold  :: [(ProductId, Int)]
   , creationTime  :: Int -- ^ '%s' unix epoch
   } deriving (Eq, Show, Read, Generic, ElmType)

-- To/FromJSON instances

instance ToJSON Response
instance FromJSON Response
instance ToJSON Request
instance FromJSON Request
instance ToJSON Product
instance FromJSON Product
instance ToJSON Planogram
instance FromJSON Planogram
instance ToJSON ProductShelfLocation
instance FromJSON ProductShelfLocation
instance ToJSON LocationOnShelf
instance FromJSON LocationOnShelf
instance ToJSON Report
instance FromJSON Report
