module Main exposing (..)

--Main code goes here

import Browser
import Html exposing (Html, Attribute, div, input, textarea, text, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import Markdown.Block exposing (..)
import Markdown.Inline exposing (..)

import Markdown.Block

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

generateLatex : String -> String
generateLatex content = 
  let latexBody = Markdown.Block.parse Nothing content |> renderBlocks
      latexHeader = """
\\documentclass{article}
%\\usepackage{ulem}     %Used for strikethrough
\\usepackage{hyperref} %Used for hyperlink creation
\usepackage[parfill]{parskip}
\\usepackage{listings}
\\usepackage{xcolor}
\\lstdefinestyle{customc}{
  belowcaptionskip=1\\baselineskip,
  breaklines=true,
  frame=L,
  xleftmargin=\\parindent,
  language=C,
  showstringspaces=false,
  basicstyle=\\footnotesize\\ttfamily,
  keywordstyle=\\bfseries\\color{green!40!black},
  commentstyle=\\itshape\\color{purple!40!black},
  identifierstyle=\\color{blue},
  stringstyle=\\color{orange},
}
\\lstset{style=customc}

\\begin{document}
"""
      latexFooter = """
\\end{document}
      """
  in
    latexHeader ++ latexBody ++ latexFooter

renderHeading : Int -> List (Inline i) -> String
renderHeading depth content =
  case depth of
    1 -> "\\section{" ++ renderInlines content ++ "}"
    2 -> "\\subsection{" ++ renderInlines content ++ "}"
    3 -> "\\subsubsection{" ++ renderInlines content ++ "}"
    4 -> "\\subsubsection{" ++ renderInlines content ++ "}" --...
    _ -> "" --Oops 

renderCodeBlock : CodeBlock -> String -> String
renderCodeBlock codeBlock code =
  case codeBlock of
    Indented -> code
    Fenced isOpen fence -> 
      "\\begin{lstlisting}" ++
        (case fence.language of
          Nothing -> "\n"
          Just language -> "[language=" ++ language ++ "]\n")
      ++ code
      ++ "\n\\end{lstlisting}\n"

renderListBlock : ListBlock -> List String -> String
renderListBlock listBlock contents = 
  let 
    type_ = 
      case listBlock.type_ of  
        Unordered -> "itemize"
        Ordered _ -> "enumerate"
    header = 
      case listBlock.type_ of
        Unordered -> "\\begin{itemize}\n"
        Ordered start -> "\\begin{enumerate}\n\\setcounter{enumi}{" ++ String.fromInt start ++ "}\n"
    content = (List.map (\line -> "\\item " ++ line ++ "\n") contents |> String.concat)
    footer = "\\end{" ++ type_ ++ "}\n"
  in
    header ++ content ++ footer

renderBlocks : List (Block b i) -> String
renderBlocks blocks = List.map renderBlock blocks |> String.concat

renderBlock : Block b i -> String
renderBlock block = 
  case block of
    BlankLine str -> "\n\n"
    ThematicBreak -> "\\rule{\\textwidth}{0.4pt}"
    Heading _ level inlines -> renderHeading level inlines
    CodeBlock codeBlock code -> renderCodeBlock codeBlock code --"codeblock: " ++ code
    Paragraph _ content -> renderInlines content
    BlockQuote content -> "\\begin{quote}\n" ++ (renderBlocks content) ++ "\n\\end{quote}"
    List listBlock contents -> renderListBlock listBlock (List.map renderBlocks contents)
    PlainInlines inlines -> renderInlines inlines
    Markdown.Block.Custom _ _ -> "custom"

renderInlines : List (Inline i) -> String
renderInlines inlines = List.map renderInline inlines |> String.concat

renderInline : Inline i -> String
renderInline inline = 
  case inline of
    Text str -> str
    HardLineBreak -> "brk"
    CodeInline str -> "\\verb|" ++ str ++ "|"
    Link url _ inlines -> "\\href{" ++ url ++ "}{" ++ renderInlines inlines ++ "}"
    Image src _ _ -> "img"
    HtmlInline _ _ _ -> "inlineHTML"
    Emphasis length content -> 
      case length of 
        1 -> "\\textif{" ++ renderInlines content ++ "}" --italics
        2 -> "\\textbf{" ++ renderInlines content ++ "}" --bold
        _ -> renderInlines content
    Markdown.Inline.Custom _ _ -> "custom"

-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ 
      textarea [ placeholder "Markdown Text", value model.mdIn, onInput Change, rows 25, cols 80 ] []
      , textarea [ rows 40, cols 80 ] [ text (model.latexOut) ]
    ]

