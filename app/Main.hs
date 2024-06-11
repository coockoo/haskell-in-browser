{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Blaze.Html5 as H
import Text.Blaze.Renderer.Utf8 (renderHtml)
import Network.Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)

application :: p -> (Response -> b) -> b
application _ respond = respond $
  responseLBS status200 [("Content-Type", "text/html")] (renderHtml rootHtml)

rootHtml :: Html
rootHtml = docTypeHtml $ do
  H.head $ do
    H.title "Hello, BlazeHtml!"
  H.body $ do
    H.h1 "Hello, BlazeHtml!"
    H.p "This is a BlazeHtml page."

main :: IO ()
main = run 3000 application
