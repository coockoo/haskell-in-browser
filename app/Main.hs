module Main where

import Data.Text
import Network.HTTP.Types (status200)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

application :: Request -> (Response -> b) -> b
application req respond = respond $ router $ pathInfo req

router :: [Text] -> Response
router ["bundle.js"] = responseFile status200 [("Content-Type", "application/javascript")] "bundle.js" Nothing
router ["Counter.wasm"] = responseFile status200 [("Content-Type", "application/wasm")] "Counter.wasm" Nothing
router _ = responseLBS status200 [("Content-Type", "text/html")] $ renderHtml rootHtml

rootHtml :: Html
rootHtml = docTypeHtml $ do
  H.head $ do
    H.title "Hello, BlazeHtml!"
  H.body $ do
    H.h1 "Hello, BlazeHtml!"
    H.p ! class_ "hehe" $ "This is a BlazeHtml page."
    H.script ! A.type_ "module" ! A.src "bundle.js" $ ""

main :: IO ()
main = run 3000 application
