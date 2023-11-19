docker run -it -v .:/ccbmk2_web -w /ccbmk2_web ccbmk2_elm elm-css ccbmk2/elm-src/css/Stylesheets.elm --output=static/css
docker run -it -v .:/ccbmk2_web -w /ccbmk2_web ccbmk2_elm elm-make ccbmk2/elm-src/Main.elm --output=static/elm/builder.js
