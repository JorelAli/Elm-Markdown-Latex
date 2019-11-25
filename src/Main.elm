module Main exposing (..)

--Main code goes here

import Browser
import Html exposing (Html, Attribute, div, input, textarea, text, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import Markdown.Elm
import Markdown.Option
import Parse exposing (BlockContent)
import MDInline exposing (..)
import Tree exposing (Tree)

-- MAIN
main =
  Browser.sandbox { 
      init = init
      , update = update
      , view = view 
      }

-- MODEL
type alias Model = { 
    mdIn : String
    , latexOut : String
  }

init : Model
init =
  { mdIn = "", latexOut = "" }

-- UPDATE
type Msg = Change String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | mdIn = newContent, latexOut = generateLatex newContent }

{-
Package appendices:



-}

generateLatex : String -> String
generateLatex content = 
  let latexBody = Parse.toMDBlockTree Markdown.Option.Extended content |> convertMDBlocksToString
      latexHeader = """
\\documentclass{article}
\\usepackage{ulem}     %Used for strikethrough
\\usepackage{hyperref} %Used for hyperlink creation

\\begin{document}
      """
      latexFooter = """
\\end{document}
      """
  in
    latexHeader ++ latexBody ++ latexFooter

convertMDBlocksToString : Tree Parse.MDBlock -> String
convertMDBlocksToString tree =
  String.concat (List.map convertMDBlock (Tree.flatten tree))

convertMDBlock : Parse.MDBlock -> String
convertMDBlock mdBlock = 
  case mdBlock of
    Parse.MDBlock blockType level content -> 
      case content of
        Parse.M inline -> convertMDInline inline
        Parse.T string -> string

convertMDInline : MDInline -> String
convertMDInline inline =
  case inline of
    OrdinaryText str      -> str
    ItalicText str        -> "\\textit{" ++ str ++ "}"
    BoldText str          -> "\\textbf{" ++ str ++ "}"
    Code str              -> "\\verb|" ++ str ++ "|" --TODO: Handle | characters in code
    InlineMath str        -> "$" ++ str ++ "$"
    StrikeThroughText str -> "STRK: " ++ str --"\\sout{" ++ str ++ "}"
    BracketedText str     -> "[" ++ str ++ "]"
    Link text url         -> "\\href{" ++ url ++ "}{" ++ text ++ "}"
    Image url text        -> "img: " ++ url ++ ", " ++ text
    Line list             -> String.concat (List.map convertMDInline list)
    Paragraph list        -> "\n\n" ++ String.concat (List.map convertMDInline list)
    Stanza str            -> "stanza:" ++ str
    Error list            -> "err"



-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ 
      textarea [ placeholder "Markdown Text", value model.mdIn, onInput Change, rows 25, cols 80 ] []
      , textarea [ rows 25, cols 80 ] [ text (model.latexOut) ]
    ]

