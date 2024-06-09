main = putStrLn (wrapHtml "My simple webpage" "Hello, World!")

html_ content = "<html>" <> content <> "</html>"
body_ content = "<body>" <> content <> "</body>"
head_ content = "<head>" <> content <> "</head>"
title_ content = "<title>" <> content <> "</title>"

wrapHtml title body = html_ (head_ (title_ title) <> body_ body)
