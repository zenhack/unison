module Unison.Styles where

import Graphics.Element as E
import Unison.Layout (Layout)
import Unison.Layout as L

body : Style
body =
  { typeface = [ "Lato", "latin" ]
  , height   = Just 16
  , color    = black
  , bold     = False
  , italic   = False
  , line     = Nothing }

code : Style
code =
  { typeface = [ "Inconsolata", "monospace", "latin" ]
  , height   = Just 16
  , color    = black
  , bold     = False
  , italic   = False
  , line     = Nothing }

codeText : String -> Element
codeText s = leftAligned (style body (toText s))

cells : k -> Element -> [Layout k] -> Layout k
cells k ifEmpty ls = case L.row ls of
  [] -> L.embed k ifEmpty
  h :: _ -> let vline = L.embed k (E.spacer 1 (L.heightOf h) |> E.color silver)
            in L.outline silver 1 (L.intersperseHorizontal vline ls)

verticalCells : k -> Element -> [Layout k] -> Layout k
verticalCells k ifEmpty ls = case L.column ls of
  [] -> L.embed k ifEmpty
  h :: _ -> let hline = L.embed k (E.spacer (L.widthOf h) 1 |> E.color silver)
            in L.outline silver 1 (L.intersperseHorizontal hline ls)

bg = white

-- http://flatuicolors.com/
turquoise = rgb 26 188 156
greenSea = rgb 22 160 133
sunFlowers = rgb 241 196 15
orange = rgb 243 156 18
emerald = rgb 46 204 113
nephritis = rgb 39 174 96
carrot = rgb 230 126 34
pumpkin = rgb 211 84 0
peterRiver = rgb 52 152 219
belizeHole = rgb 41 128 185
alizarin = rgb 231 76 60
pomegranate = rgb 192 57 43
amethyst = rgb 155 89 182
wisteria = rgb 142 68 173
clouds = rgb 236 240 241
silver = rgb 189 195 199
wetAsphalt = rgb 52 73 94
midnightBlue = rgb 44 62 80
concrete = rgb 149 165 166
asbestos = rgb 127 140 141
