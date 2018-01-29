{-# LANGUAGE DeriveGeneric #-}
module Db
   ( Db(..), initDb, reloadDb, readDb, createReport ) where

import           Api
import           Control.Concurrent.MVar
import qualified Data.ByteString.Lazy as LB
import qualified Data.Aeson as A
import           Data.Monoid ((<>))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IMap
import           System.IO.Unsafe (unsafePerformIO)
import           System.FilePath ((</>), (<.>))
import qualified System.Directory as Directory
import           GHC.Generics (Generic)
import qualified Data.List as L

-- | This is stored as follows:
--
-- @
-- path_to_db_dir/products_*.json               :: [Product]
-- path_to_db_dir/stores.json                   :: Map String [Int]
-- path_to_db_dir/report_%{user}_${time}.json   :: Report
-- @
data Db = Db
   { products :: IntMap Product -- id: ProductId
   , stores   :: Map String [Int]
   , reports  :: [Report]
   } deriving (Eq, Show, Generic)

globalDb :: MVar (FilePath, Db)
globalDb = unsafePerformIO newEmptyMVar
{-# NOINLINE globalDb #-}

-- | @initDb "path-to-db-dir"@ Initialize the global database. (Call only
-- ever once.)
initDb :: FilePath -> IO ()
initDb dbDirectory = do
   Directory.createDirectoryIfMissing True dbDirectory
   putMVar globalDb (dbDirectory, Db mempty mempty [])
   readReports
   readStores
   readProducts

reloadDb :: IO ()
reloadDb = readStores >> readProducts

-- | Read-only data action.
readDb :: (Db -> a) -> IO a
readDb f = f . snd <$> readMVar globalDb

-- | Create a report, write to db and disk.
createReport :: Report -> IO ()
createReport report = modifyMVar_ globalDb $ \(dir, db) -> do
   let fileName = dir </> "report_" <> planogramUser report <> show (creationTime report) <.> "json"
   putStrLn $ "Create report: " ++ fileName
   LB.writeFile fileName (A.encode report)
   return (dir, db { reports = report : reports db })

-- | @dir/report_*.json@
readReports :: IO ()
readReports = modifyMVar_ globalDb $ \(dir, db) -> do
   paths <- Directory.listDirectory dir
   let paths' = [ dir </> path | path <- paths, "report_" `L.isPrefixOf` path, ".json" `L.isSuffixOf` path ]
   allReports <- mapM fromFile paths' :: IO [Report]
   putStrLn $ "Read " ++ show (length allReports) ++ " reports from " ++ show paths'
   return (dir, db { reports = allReports })

-- | @dir/products_*.json@
readProducts :: IO ()
readProducts = modifyMVar_ globalDb $ \(dir, db) -> do
   paths <- Directory.listDirectory dir
   let paths' = [ dir </> path | path <- paths, "products_" `L.isPrefixOf` path, ".json" `L.isSuffixOf` path ]
   allProducts <- concat <$> mapM fromFile paths' :: IO [Product]
   putStrLn $ "Read " ++ show (length allProducts) ++ " products from " ++ show paths'
   return (dir, db { products = IMap.fromList (map ((,) <$> ident <*> id) allProducts) })

readStores :: IO ()
readStores = modifyMVar_ globalDb $ \(dir, db) -> do
   allStores <- fromFile (dir </> "stores.json")
   putStrLn $ "Read stores: " ++ show (Map.keys allStores)
   return (dir, db { stores = allStores })

-- | Read file contents to JSON.
fromFile :: A.FromJSON a => FilePath -> IO a
fromFile path = LB.readFile path >>= either error return . A.eitherDecode'
