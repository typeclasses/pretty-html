Example:

```haskell
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
```

Generates the following HTML output:

```html
<!doctype html>
<!-- This is an HTML document. -->
<html lang="en">
	<!--
		A document consists of two main parts:
		  1. The head, containing metadata
		  2. The body, containing the visible content.
	-->
	<head>
		<title>Example document</title>
		<meta name="viewport" content="width=device-width, initial-scale=1">
		<meta property="description" content="This &lt;html&gt; was &quot;generated&quot; with Haskell.">
		<link rel="stylesheet" type="text/css" href="../examples.css">
	</head>
	<body>
		<h1>The example doc</h1>
		<p>A list of uninteresting facts:</p>
		<ol>
			<li>4 &lt; 7
			<li>Sometimes <span style="font-weight: bold">emboldened</span> text is used for emphasis.
		</ol>
		<p>Please sign my guest book.</p>
		<form method="POST" action="/cgi-bin/guestbook.php">
			<textarea name="message" autofocus>
			<br>
			<button type="submit">Submit</button>
		</form>
	</body>
</html>
```
