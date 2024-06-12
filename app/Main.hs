module Main where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Network.Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)

application :: Request -> (Response -> b) -> b
application req respond = respond $
  case pathInfo req of
    ["bundle.js"] -> responseFile status200 [("Content-Type", "application/javascript")] "bundle.js" Nothing
    _ -> responseLBS status200 [("Content-Type", "text/html")] $ renderHtml rootHtml

rootHtml :: Html
rootHtml = docTypeHtml $ do
  H.head $ do
    H.title "Hello, BlazeHtml!"
  H.body $ do
    H.h1 "Hello, BlazeHtml!"
    H.p ! class_ "hehe" $ "This is a BlazeHtml page."
    H.script ! A.src "bundle.js" $ ""

main :: IO ()
main = run 3000 application
