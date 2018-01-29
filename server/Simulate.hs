{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Simulate where

import           Api
import           Control.Monad
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IMap

-- ^ is sensible?
validate :: Planogram -> IntMap Product -> [Int] -> Either String ()
validate Planogram{..} products shelves = do
   -- all product ids valid
   forM_ (productId <$> productShelfLocations) $ \i ->
      unless (any ((==) i . ident) products) $ Left ("unknown product: " ++ show i)

   -- not over-filling shelves
   forM_ (zip [0..] shelves) $ \(sId, shelfLen) ->
      forM_ [Top, Middle, Bottom] $ \loc ->
         let submittedLength = sum
                [ width product
                | ProductShelfLocation p s l <- productShelfLocations
                , s == sId, l == loc, let Just product = IMap.lookup p products ]
         in unless (submittedLength <= shelfLen)
            $ Left ("Over-filled: " ++ show sId ++ " " ++ show loc)

getProfitProducts :: IntMap Product -> [(ProductId, Int)] -> Double
getProfitProducts products productsSold = sum $ do
   (i, count) <- productsSold
   let Just product = IMap.lookup i products
   return (priceSell product - priceBuy product)

evaluate :: Planogram -> IntMap Product -> [Int] -> [(ProductId, Int)]
evaluate planogram products shelves = [] -- methods not disclosed (yet) :)
