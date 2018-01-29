{-# LANGUAGE RecordWildCards #-}
module Main where

import           Api
import           Data.Aeson (eitherDecode, encode)
import           Control.Monad (forever)
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Network.WebSockets as WS
import           System.Environment (getArgs)

main :: IO ()
main = do
   host : port_s : _ <- getArgs
   let port = read port_s
   WS.runClient host port "/" $ \conn -> do
      putStrLn "Connected!"

      debugLoop conn

sendRequest :: WS.Connection -> Request -> IO ()
sendRequest conn req = WS.sendTextData conn (encode req)

receiveResponse :: WS.Connection -> IO Response
receiveResponse conn = do
   incoming <- WS.receiveData conn
   case eitherDecode incoming :: Either String Response of
     Left err -> putStrLn err >> receiveResponse conn
     Right res -> return res

prettyResponse :: Response -> String
prettyResponse (AllProducts xs) = unlines $
   [ show (ident, width, priceBuy, priceSell, attributes) | Product{..} <- xs ]
prettyResponse (Reports xs) = unlines $
   [ "user: " ++ planogramUser ++ ", store: " ++ planogramStore planogram ++ ", time: " ++ show creationTime
      ++ "\n      profit: " ++ show profit
      ++ "\n  placements: " ++ show [ (p, s, l) | ProductShelfLocation p s l <- productShelfLocations planogram ]
      ++ "\n       sells: " ++ show productsSold
   | Report{..} <- xs ]
prettyResponse other = show other

debugLoop :: WS.Connection -> IO ()
debugLoop conn = forever $ do
   req <- readLn
   sendRequest conn req
   case req of
      Identify{} -> return ()
      _          -> receiveResponse conn >>= putStrLn . prettyResponse
