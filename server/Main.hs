{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import           Api
import           Db
import           Simulate as Simulate
import           Control.Monad (forever)
import           Control.Exception (catch)
import           Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.Monoid ((<>))
import qualified Data.Map as Map
import qualified Data.IntMap as IMap
import           Data.Time (defaultTimeLocale, getCurrentTime, formatTime)
import           System.Environment (getArgs)
import           System.IO (stdout)
import           Network.HTTP.Types (status400)
import qualified Network.WebSockets as WS
import qualified Network.Wai as Wai
import           Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.Wai.Handler.Warp as Warp

isValidUserId :: String -> Bool
isValidUserId uid
   | any (`notElem` (['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'])) uid = False
   | length uid > 20 = False
   | length uid < 3  = False
   | otherwise       = True

main :: IO ()
main = do
   initDb "./planogram-db/"
   runWs

runWs :: IO ()
runWs = do
   [port_s] <- getArgs
   let port = read port_s
   Warp.run port app
 where
    app = websocketsOr WS.defaultConnectionOptions wsApp backupApp
    backupApp _ respond = respond $ Wai.responseLBS status400 [] "Not a WebSocket request"

wsApp :: WS.ServerApp
wsApp pending = do
   conn <- WS.acceptRequest pending
   WS.forkPingThread conn 30
   handleConn "anonymous" conn
      `catch` \(_ :: WS.ConnectionException) -> return ()

handleConn :: String -> WS.Connection -> IO ()
handleConn ident conn = do
   received <- WS.receiveData conn
   case eitherDecode received of
      Left err -> putStrLn err
      Right InternalRefresh -> reloadDb >> handleConn ident conn
      Right (Identify ident')
         | isValidUserId ident' -> handleConn ident' conn
         | otherwise            -> WS.sendTextData conn (encode $ ErrorMsg "Invalid user id")
      Right request -> do
         response <- handleRequest ident conn request
         LB.hPut stdout $
            "Client:   " <> C8.pack ident   <> "\n" <>
            "Request:  " <> received        <> "\n" <>
            "Response: " <> encode response <> "\n"
         WS.sendTextData conn (encode response)
         handleConn ident conn

handleRequest :: String -> WS.Connection -> Request -> IO Response
handleRequest ident conn request = case request of
   Identify{}       -> error "handleRequest/Identity: never reached"
   GetProducts      -> AllProducts <$> readDb (IMap.elems . products)
   GetReports       -> Reports <$> readDb reports
   GetStore storeId -> maybe (ErrorMsg "no such store") (StoreShelfSpace storeId) <$> readDb (Map.lookup storeId . stores)
   SubmitPlanogram planogram
      | ident == "anonymous" -> return $ ErrorMsg "Don't submit planograms as anonymous (send 'Identity' first)"
      | otherwise ->
         readDb (Map.lookup (planogramStore planogram) . stores) >>= \case
            Nothing -> return $ ErrorMsg "no such store"
            Just shelves -> do
               db <- readDb id
               case Simulate.validate planogram (products db) shelves of
                  Right () -> do
                     epoch <- read . formatTime defaultTimeLocale "%s" <$> getCurrentTime
                     let sold   = Simulate.evaluate planogram (products db) shelves
                         profit = getProfitProducts (products db) sold
                         report = Report ident planogram profit sold epoch
                     createReport report
                     return (Reports [report])
                  Left str ->
                     return (ErrorMsg $ "Planogram invalid: " ++ str)
