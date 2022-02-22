{- |

Text elements and attribute values are escaped.
Tag names, attribute keys, and comments are neither escaped
nor validated; it is possible to construct malformed documents.

Build an 'Html' value using 'tag', 'tag'', 'attr', 'attr'', 'text',
'comment', and 'inline'. Then use 'runHtml' to get a 'TextBuilder'.

'Html' has a 'Monoid' instance. It is often more convenient to apply
'fold' a list literal than to chain the '(<>)' operator. You can use
'mempty' when you need an HTML tag with an empty body.

-}
module PrettyHTML
  (
    {- * The Html type -}  Html, runHtml,
    {- * Tags          -}  tag, tag',
    {- * Attributes    -}  Attr, attr, attr',
    {- * Plain text    -}  text,
    {- * Comments      -}  comment,
    {- * Formatting    -}  inline,
    {- * Text aliases  -}  LazyText, TextBuilder
  ) where

import Foldable (fold)
import Natural (Natural)
import Semigroup (stimes)

import qualified TextBuilder
import qualified LazyText

type LazyText = LazyText.Text
type TextBuilder = TextBuilder.Builder

data Html = NoHtml | Html (Context -> TextBuilder)

runHtml :: Html -> TextBuilder
runHtml = \case
    NoHtml -> mempty
    Html f -> f (Block 0)

instance Semigroup Html where
    NoHtml <> x = x
    x <> NoHtml = x
    Html f <> Html g = Html \c -> f c <> g c

instance Monoid Html where
    mempty = NoHtml

data Context = Block Natural | Inline

-- | See 'attr' and 'attr''.
type Attr = (LazyText, Maybe LazyText)

raw :: TextBuilder -> Html
raw x = Html \_ -> x

indent :: Html
indent = Html \case
    Block 0 -> mempty
    Block n -> stimes n "\t"
    Inline -> mempty

endOfLine :: Html
endOfLine = Html \case
    Block _ -> "\n"
    Inline -> mempty

changeContext :: (Context -> Context) -> Html -> Html
changeContext f = \case
    NoHtml -> NoHtml
    Html g -> Html (g . f)

changeIndent :: (Natural -> Natural) -> Context -> Context
changeIndent f = \case
    Block n -> Block (f n)
    Inline -> Inline

-- | Content wrapped in 'inline' is rendered tersely, without line breaks and indentation.
--
-- Typically paragraphs should be rendered inline,
-- e.g. @'inline' ('tag' "p" [] $ 'fold' [ _, _, _ ])@
inline :: Html -> Html
inline x = fold
    [ indent
    , changeContext (const Inline) x
    , endOfLine
    ]

-- | Attribute with a key and value.
attr ::
    LazyText -- ^ Attribute key. This must be a valid key; no checking or escaping is done.
    -> LazyText -- ^ Attribute value. This can safely be anything; it will be escaped.
    -> Attr
attr k v = (k, Just v)

-- | Attribute without a value, e.g. "autofocus", "checked".
attr' ::
    LazyText -- ^ Attribute key. This must be a valid key; no checking or escaping is done.
    -> Attr
attr' x = (x, Nothing)

-- | Element with opening and closing tags.
tag :: Foldable t =>
    LazyText -- ^ Tag name. This must be a valid tag name; no checking or escaping is done.
    -> t Attr -- ^ List of attributes. See 'attr' and 'attr''.
    -> Html -- ^ Content nested within the tag.
    -> Html
tag x a mi = fold
    [ indent
    , (raw . fold)
        [ "<"
        , TextBuilder.fromLazyText x
        , foldMap buildAttr a
        , ">"
        ]
    , case mi of
        NoHtml -> mempty
        i -> fold
            [ endOfLine
            , changeContext (changeIndent (+ 1)) i
            , indent
            ]
    , (raw . fold)
        [ "</"
        , TextBuilder.fromLazyText x
        , ">"
        ]
    , endOfLine
    ]

-- | Self-closing tag, e.g. "meta", "br", "li".
tag' :: Foldable t =>
    LazyText -- ^ Tag name. This must be a valid tag name; no checking or escaping is done.
    -> t Attr -- ^ List of attributes. See 'attr' and 'attr''.
    -> Html
tag' x a = fold
    [ indent
    , (raw . fold)
        [ "<"
        , TextBuilder.fromLazyText x
        , foldMap buildAttr a
        , ">"
        ]
    , endOfLine
    ]

buildAttr :: Attr -> TextBuilder
buildAttr (k, vMaybe) = fold
    [ " "
    , TextBuilder.fromLazyText k
    , foldMap attrValue vMaybe
    ]

attrValue :: LazyText -> TextBuilder
attrValue v = fold
    [ "=\""
    , foldMap attrEscape (LazyText.unpack v)
    , "\""
    ]

attrEscape :: Char -> TextBuilder
attrEscape = \case
    '<' -> "&lt;"
    '>' -> "gt;"
    '&' -> "&amp;"
    '"' -> "&quot;"
    c -> TextBuilder.singleton c

-- | A text element.
text ::
    LazyText -- ^ Can safely be anything; it will be escaped.
    -> Html
text x = fold
    [ indent
    , raw (foldMap textEscape (LazyText.unpack x))
    , endOfLine
    ]

textEscape :: Char -> TextBuilder
textEscape = \case
    '<' -> "&lt;"
    '>' -> "gt;"
    '&' -> "&amp;"
    c -> TextBuilder.singleton c

-- | A comment appears in the HTML source code but is ignored when read.
comment ::
    LazyText -- ^ Must not contain a comment closing tag; this is neither checked nor escaped.
    -> Html
comment x = Html \case
    c@Block{} -> let Html f = blockComment x in f c
    c@Inline{} -> let Html f = oneLineComment x in f c

oneLineComment :: LazyText -> Html
oneLineComment x = fold
    [ indent
    , raw "<!-- "
    , changeContext (const Inline) (commentBody x)
    , raw " -->"
    , endOfLine
    ]

blockComment :: LazyText -> Html
blockComment i = fold
    [ indent
    , raw "<!--"
    , endOfLine
    , changeContext (changeIndent (+ 1)) (commentBody i)
    , indent
    , raw "-->"
    , endOfLine
    ]

commentBody :: LazyText -> Html
commentBody = foldMap commentLine . LazyText.lines

commentLine :: LazyText -> Html
commentLine l = fold
    [ indent
    , raw (TextBuilder.fromLazyText l)
    , endOfLine
    ]
