module Main (main) where

import PrettyHTML

import FilePath ((</>))
import Foldable (fold)
import Exit (exitFailure)
import LazyTextEncoding (decodeUtf8)
import Monad (when)
import Paths (getDataDir)

import qualified TextBuilder
import qualified ByteString
import qualified LazyText
import qualified LazyTextIO

main :: IO ()
main = do
    dataDir <- getDataDir
    expectedHtml <- decodeUtf8 <$> ByteString.readFile (dataDir </> "example.html")
    builtHtml <- pure $ TextBuilder.toLazyText $ runHtml document
    when (expectedHtml /= builtHtml) $ do
        LazyTextIO.putStrLn "🔥 Produced this incorrect result:"
        LazyTextIO.putStrLn builtHtml
        exitFailure

document :: Html
document =
  fold
    [ tag' "!doctype" [attr' "html"]
    , inline $ comment "This is an HTML document."
    , tag "html" [attr "lang" "en"] $
        fold
          [ comment $ LazyText.unlines
              [ "A document consists of two main parts:"
              , "  1. The head, containing metadata"
              , "  2. The body, containing the visible content."
              ]
          , tag "head" [] $ fold
              [ inline $ tag "title" [] $ text "Example document"
              , tag' "meta"
                  [ attr "name" "viewport"
                  , attr "content" "width=device-width, initial-scale=1"
                  ]
              , tag' "meta"
                  [ attr "property" "description"
                  , attr "content" "This <html> was \"generated\" with Haskell."
                  ]
              , tag' "link"
                  [ attr "rel" "stylesheet"
                  , attr "type" "text/css"
                  , attr "href" "../examples.css"
                  ]
              ]
          , tag "body" [] $ fold
              [ inline $ tag "h1" [] $ text "The example doc"
              , inline $ tag "p" [] $ text "A list of uninteresting facts:"
              , tag "ol" [] $ fold
                  [ inline $ fold
                      [ tag' "li" []
                      , text "4 < 7"
                      ]
                  , inline $ fold
                      [ tag' "li" []
                      , text "Sometimes "
                      , tag "span" [attr "style" "font-weight: bold"] $
                          text "emboldened"
                      , text " text is used for emphasis."
                      ]
                  ]
              , inline $ tag "p" [] $ text "Please sign my guest book."
              , tag "form" [ attr "method" "POST"
                           , attr "action" "/cgi-bin/guestbook.php" ] $ fold
                  [ tag' "textarea" [attr "name" "message", attr' "autofocus"]
                  , tag' "br" []
                  , inline $ tag "button" [attr "type" "submit"] $ text "Submit"
                  ]
              ]
          ]
    ]
