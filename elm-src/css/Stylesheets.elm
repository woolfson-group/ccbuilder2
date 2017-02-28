port module Stylesheets exposing (..)

import Css.File exposing (CssFileStructure, CssCompilerProgram)
import BuilderCss


port files : CssFileStructure -> Cmd msg


fileStructure : CssFileStructure
fileStructure =
  Css.File.toFileStructure
    [ ( "index.css", Css.File.compile [ BuilderCss.css ] ) ]


main : CssCompilerProgram
main =
  Css.File.compiler files fileStructure