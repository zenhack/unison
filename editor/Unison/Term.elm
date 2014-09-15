module Unison.Term where

import Array
import Array (Array)
import Dict
import Dict (Dict)
import Json
import Set
import Set (Set)
import String
import Graphics.Element as E
import Graphics.Input (Handle, hoverable)
import Text(..)
import Unison.Layout (Layout)
import Unison.Layout as L
import Unison.Styles as Styles
import Unison.Styles (codeText)
import Unison.Hash (Hash)
import Unison.Hash as H
import Unison.Jsonify as J
import Unison.Jsonify (Jsonify)
import Unison.Metadata as Metadata
import Unison.Metadata (Metadata, Fixity)
import Unison.Parser as P
import Unison.Parser (Parser)
import Unison.Path (..)
import Unison.Var (I)
import Unison.Var as V
import Unison.Type as T

data Literal
  = Number Float
  | Str String
  | Vector (Array Term)

data Term
  = Var I
  | Lit Literal
  | Con Hash
  | Ref Hash
  | App Term Term
  | Ann Term T.Type
  | Lam I Term

todo : a
todo = todo

-- layout : Term -> Layout Path
render : Term -- term to render
      -> { key            : Hash
         , availableWidth : Int
         , metadata       : Hash -> Metadata }
      -> Layout { hash : Hash, path : Path, selectable : Bool }
render expr env =
  let
    md = env.metadata env.key
    tag path = { path = path, hash = env.key, selectable = True }
    space = codeText " "
    spaces n =
      if n <= 0 then empty else codeText (String.padLeft (n*2) ' ' "")
    space2 = codeText "  "
    indentWidth = E.widthOf space2
    paren : Bool -> Element -> Element
    paren parenthesize e =
      if parenthesize
      then let (opening, closing) = (codeText "(", codeText ")")
               topOpen = container (widthOf opening) (heightOf e) topLeft (codeText "(")
               bottomClose = container (widthOf closing) (heightOf e) bottomLeft (codeText ")")
           in flow right [topOpen, e, bottomClose]
      else e

    go : Bool -> Int -> Int -> { path : Path, term : Term } -> Layout { hash : Hash, path : Path, selectable : Bool }
    go allowBreak ambientPrec availableWidth cur =
      case cur.term of
        Var n -> codeText (Metadata.resolveLocal md cur.path n).name |> L.embed (tag cur.path)
        Ref h -> codeText (Metadata.firstName h (env.metadata h)) |> L.embed (tag cur.path)
        Con h -> codeText (Metadata.firstName h (env.metadata h)) |> L.embed (tag cur.path)
        Lit (Number n) -> codeText (String.show n) |> L.embed (tag cur.path)
        Lit (Str s) -> codeText ("\"" ++ s ++ "\"") |> L.embed (tag cur.path)
        _ -> let space' = L.embed (tag cur.path) space in
        case break env.key env.metadata cur.path cur.term of
          Prefix f args ->
            let f' = go False 9 availableWidth f
                lines = f' :: map (go False 10 0) args
                unbroken = L.intersperseHorizontal space' lines
                        |> L.transform (paren (ambientPrec > 9))
            in if not allowBreak || L.widthOf unbroken < availableWidth
               then unbroken
               else let args' = map (go True 10 (availableWidth - L.widthOf f' - L.widthOf space')) args
                             |> L.vertical (tag cur.path)
                    in L.intersperseHorizontal space' [f',args']
                    |> L.transform (paren (ambientPrec > 9))
          Operators leftAssoc prec hd tl ->
            let f (op,r) l = L.intersperseHorizontal space' [ l, go False 10 0 op, go False rprec 0 r ]
                unbroken = foldl f (go False lprec 0 hd) tl
                        |> L.transform (paren (ambientPrec > 9))
                lprec = if leftAssoc then prec else 1+prec
                rprec = if leftAssoc then 1+prec else prec
                bf (op,r) l =
                  let op' = go False 10 0 op
                      remWidth = availableWidth - L.widthOf op' - L.widthOf space'
                  in L.above (tag cur.path) l <|
                     L.intersperseHorizontal space' [op', go True rprec remWidth r ]
            in if not allowBreak || L.widthOf unbroken < availableWidth
               then unbroken
               else foldl bf (go True lprec (availableWidth - indentWidth) hd) tl
                    |> L.transform (paren (ambientPrec > 9))
          Lambda args body ->
            let argLayout = map (go False 0 0) args ++ [L.embed (tag cur.path) (codeText "→")]
                         |> L.intersperseHorizontal space'
                unbroken = L.intersperseHorizontal space' [argLayout, go False 0 0 body]
                        |> L.transform (paren (ambientPrec > 0))
            in if not allowBreak || L.widthOf unbroken < availableWidth
               then unbroken
               else L.above (tag cur.path)
                      argLayout
                      (L.horizontal (tag cur.path) [ space', space', go True 0 (availableWidth - indentWidth) body])
                    |> L.transform (paren (ambientPrec > 0))
          Bracketed es ->
            let unbroken = Styles.cells (tag cur.path) (codeText "[]") (map (go False 0 0) es)
            in if not allowBreak || L.widthOf unbroken < availableWidth || length es < 2
            then unbroken
            else Styles.verticalCells (tag cur.path) (codeText "[]")
                                      (map (go True 0 (availableWidth - 4)) es) -- account for cell border
  in go True 0 env.availableWidth { path = Array.empty, term = expr }

data Break a
  = Prefix a [a]          -- `Prefix f [x,y,z] == f x y z`
  | Operators Bool Int a [(a,a)] -- `Operators False x [(+,y), (+,z)] == (x + y) + z`
                                 -- `Operators True x [(^,y), (^,z)] == x ^ (y ^ z)`
  | Bracketed [a]         -- `Bracketed [x,y,z] == [x,y,z]`
  | Lambda [a] a          -- `Lambda [x,y,z] e == x -> y -> z -> e`

break : Hash -> (Hash -> Metadata) -> Path -> Term -> Break { path : Path, term : Term }
break hash md path expr =
  let prefix f acc path = case f of
        App f arg -> prefix f ({ path = path `push` Arg, term = arg } :: acc) (path `push` Fn)
        _ -> Prefix { path = path, term = f } acc
      opsL o prec e acc path = case e of
        App (App op l) r ->
          if op == o
          then
            let hd = (
              { path = path `append` [Fn,Fn], term = op },
              { path = path `push` Arg, term = r })
            in opsL o prec l (hd :: acc) (path `append` [Fn,Arg])
          else Operators False prec { path = path, term = e} acc
        _ -> Operators False prec { path = path, term = e } acc
      opsR o prec e path = case e of
        App (App op l) r ->
          if op == o
          then case opsR o prec r (path `push` Arg) of
            Operators _ prec hd tl ->
              let tl' = ({ path = path `append` [Fn,Fn], term = op }, hd) :: tl
              in Operators True prec { path = path `append` [Fn,Arg], term = l} tl'
          else Operators True prec { path = path, term = e} []
        _ -> Operators True prec { path = path, term = e } []
  in case expr of
    Lit (Vector xs) -> xs
                    |> Array.indexedMap (\i a -> { path = path `push` Index i, term = a })
                    |> Array.toList
                    |> Bracketed
    App (App op l) r ->
      let sym = case op of
        Ref h -> Metadata.firstSymbol h (md h)
        Con h -> Metadata.firstSymbol h (md h)
        Var v -> Metadata.resolveLocal (md hash) path v
      in case sym.fixity of
        Metadata.Prefix -> prefix (App (App op l) r) [] path -- not an operator chain, fall back
        Metadata.InfixL -> opsL op sym.precedence (App (App op l) r) [] path -- left associated operator chain
        Metadata.InfixR -> opsR op sym.precedence (App (App op l) r) path
    Lam v body -> case body of -- audit this
      Lam _ _ -> case break hash md (path `push` Body) body of
        Lambda args body2 -> Lambda ({ path = path `push` Body, term = body } :: args) body2
        _ -> Lambda [{path = path, term = expr }] { path = path `push` Body, term = body }
      _ -> Lambda [{path = path, term = expr }] { path = path `push` Body, term = body }
    _ -> prefix expr [] path

parseLiteral : Parser Literal
parseLiteral = P.union' <| \t ->
  if | t == "Number" -> P.map Number P.number
     | t == "String" -> P.map Str P.string
     | t == "Vector" -> P.map (Vector . Array.fromList) (P.array parseTerm)

jsonifyLiteral l = case l of
  Number n -> J.tag' "Number" J.number n
  Str s -> J.tag' "String" J.string s
  Vector es -> J.tag' "Vector" (J.contramap Array.toList (J.array jsonifyTerm)) es

parseTerm : Parser Term
parseTerm = P.union' <| \t ->
  if | t == "Var" -> P.map Var V.parse
     | t == "Lit" -> P.map Lit parseLiteral
     | t == "Con" -> P.map Con H.parse
     | t == "Ref" -> P.map Ref H.parse
     | t == "App" -> P.lift2 App parseTerm parseTerm
     | t == "Ann" -> P.lift2 Ann parseTerm T.parseType
     | t == "Lam" -> P.lift2 Lam V.parse parseTerm

jsonifyTerm : Jsonify Term
jsonifyTerm e = case e of
  Var v -> J.tag' "Var" V.jsonify v
  Lit l -> J.tag' "Lit" jsonifyLiteral l
  Con h -> J.tag' "Con" H.jsonify h
  Ref h -> J.tag' "Ref" H.jsonify h
  App f x -> J.tag' "App" (J.array jsonifyTerm) [f, x]
  Ann e t -> J.tag' "Ann" (J.tuple2 jsonifyTerm T.jsonifyType) (e, t)
  Lam n body -> J.tag' "Lam" (J.tuple2 V.jsonify jsonifyTerm) (n, body)

