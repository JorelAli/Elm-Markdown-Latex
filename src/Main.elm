module Main exposing (..)

--Main code goes here

import Browser
import Html exposing (Html, div, textarea, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

import Markdown.Block exposing (..)
import Markdown.Inline exposing (..)

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
      latexHeader = """\\documentclass{article}
\\usepackage{hyperref}         %Used for hyperlink creation
\\usepackage[parfill]{parskip} %Paragraph formatting
\\usepackage{listings}         %Code blocks
\\usepackage{xcolor}           %Code block colors
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
\\setcounter{secnumdepth}{5}

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
    1 -> "\\section{" ++ renderInlines content ++ "}\n"
    2 -> "\\subsection{" ++ renderInlines content ++ "}\n"
    3 -> "\\subsubsection{" ++ renderInlines content ++ "}\n"
    4 -> "\\paragraph{" ++ renderInlines content ++ "}\\mbox{}\\\\\n" --...
    _ -> "" --Oops 

renderCodeBlock : CodeBlock -> String -> String
renderCodeBlock codeBlock code =
  case codeBlock of
    Indented -> code
    Fenced _ fence -> 
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
        Ordered start -> "\\begin{enumerate}\n\\setcounter{enumi}{" ++ String.fromInt (start - 1) ++ "}\n"
    content = List.map (\line -> "\\item " ++ line ++ "\n") contents |> String.concat
    footer = "\\end{" ++ type_ ++ "}\n"
  in
    header ++ content ++ footer

renderBlocks : List (Block b i) -> String
renderBlocks blocks = List.map renderBlock blocks |> String.concat

renderBlock : Block b i -> String
renderBlock block = 
  case block of
    BlankLine _ -> "\n\n"
    ThematicBreak -> "\\rule{\\textwidth}{0.4pt}"
    Heading _ level inlines -> renderHeading level inlines
    CodeBlock codeBlock code -> renderCodeBlock codeBlock code --"codeblock: " ++ code
    Paragraph _ content -> renderInlines content
    BlockQuote content -> "\\begin{quote}\n" ++ renderBlocks content ++ "\n\\end{quote}"
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
    Image _ _ _ -> "img"
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
      div [ id "container" ] [
        div [ class "alert alert-primary" ] [ text "Enter markdown here" ]
        , textarea [ value model.mdIn, onInput Change, rows 25, id "md", class "form-control" ] []
      ],
      div [ id "container" ] [
        div [ class "alert alert-primary" ] [ text "Get LaTeX here" ]
        , textarea [ rows 25, id "latex", readonly True, class "form-control" ] [ text model.latexOut ]
      ]
    ]

