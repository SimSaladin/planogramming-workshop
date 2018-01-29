{-# LANGUAGE OverloadedStrings #-}
module Main where

import Api

import Data.Proxy
import Elm

spec :: Spec
spec = Spec
   [ "Api" ]
   [ "import Json.Encode"
   , "import Json.Decode exposing (..)"
   , "import Json.Decode.Pipeline exposing (..)"
   , "import Exts.Json.Encode"
   , toElmTypeSource    (Proxy :: Proxy Request)
   , toElmEncoderSource (Proxy :: Proxy Request)

   , toElmTypeSource    (Proxy :: Proxy Response)
   , toElmDecoderSource (Proxy :: Proxy Response)

   , toElmTypeSource    (Proxy :: Proxy Product)
   , toElmDecoderSource (Proxy :: Proxy Product)
   , toElmEncoderSource (Proxy :: Proxy Product)

   , toElmTypeSource    (Proxy :: Proxy Report)
   , toElmDecoderSource (Proxy :: Proxy Report)
   , toElmEncoderSource (Proxy :: Proxy Report)

   , toElmTypeSource    (Proxy :: Proxy Planogram)
   , toElmDecoderSource (Proxy :: Proxy Planogram)
   , toElmEncoderSource (Proxy :: Proxy Planogram)

   , toElmTypeSource    (Proxy :: Proxy LocationOnShelf)
   , toElmDecoderSource (Proxy :: Proxy LocationOnShelf)
   , toElmEncoderSource (Proxy :: Proxy LocationOnShelf)

   , toElmTypeSource    (Proxy :: Proxy ProductShelfLocation)
   , toElmDecoderSource (Proxy :: Proxy ProductShelfLocation)
   , toElmEncoderSource (Proxy :: Proxy ProductShelfLocation)
   ]

main = specsToDir [spec] "./client/src"
