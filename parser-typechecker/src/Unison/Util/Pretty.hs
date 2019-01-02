{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Unison.Util.Pretty (
   Pretty, Pretty0,
   bulleted,
   -- breakable
   column2,
   commas,
   oxfordCommas,
   dashed,
   external,
   flatMap,
   fromPretty0,
   group,
   hang',
   hang,
   hangUngrouped',
   hangUngrouped,
   indent,
   indentAfterNewline,
   indentN,
   indentNAfterNewline,
   leftPad,
   lines,
   linesSpaced,
   lit,
   map,
   nest,
   newline,
   numbered,
   orElse,
   orElses,
   parenthesize,
   parenthesizeCommas,
   parenthesizeIf,
   preferredWidth,
   preferredHeight,
   render,
   renderUnbroken,
   rightPad,
   sep,
   sepSpaced,
   softbreak,
   spaceIfBreak,
   spacesIfBreak,
   spaced,
   spacedMap,
   surroundCommas,
   text,
   toANSI,
   toPlain,
   wrap,
   wrapString,
   black, red, green, yellow, blue, purple, cyan, white, hiBlack, hiRed, hiGreen, hiYellow, hiBlue, hiPurple, hiCyan, hiWhite, bold
  ) where

import           Data.Char                      ( isSpace )
import           Data.Foldable                  ( toList )
import           Data.List                      ( foldl' , foldr1, intersperse )
import           Data.Sequence                  ( Seq )
import           Data.String                    ( IsString , fromString )
import           Data.Text                      ( Text )
import           Prelude                 hiding ( lines , map )
import qualified Unison.Util.ColorText         as CT
import           Unison.Util.Monoid             ( intercalateMap )
import qualified Data.ListLike                 as LL
import qualified Data.Sequence                 as Seq
import qualified Data.Text                     as Text

type Width = Int

data Pretty s = Pretty { delta :: Delta, out :: F s (Pretty s) }

instance Functor Pretty where
  fmap f (Pretty d o) = Pretty d (mapLit f $ fmap (fmap f) o)

data F s r
  = Empty | Group r | Lit s | Wrap (Seq r) | OrElse r r | Append (Seq r)
  deriving (Show, Foldable, Traversable, Functor)

data Pretty0 e s
  = Empty0 | OrElse0 (Pretty0 e s) (Pretty0 e s) | Group0 (Pretty0 e s)
  | External0 e | Lit0 s
  | Hang0 (Pretty0 e s) (Pretty0 e s) (Pretty0 e s)
  | HangUngrouped0 (Pretty0 e s) (Pretty0 e s) (Pretty0 e s)
  | Wrap0 (Pretty0 e s) | Append0 (Seq (Pretty0 e s))
  | TwoColumn0 [(Pretty0 e s, Pretty0 e s)]
  | IndentAfterNewline0 (Pretty0 e s) (Pretty0 e s)
  deriving (Functor, Foldable, Traversable, Show)

instance Semigroup (Pretty0 e s) where (<>) = mappend

instance Monoid (Pretty0 e s) where
  mempty = Empty0
  mappend (Append0 ps) (Append0 ps2) = Append0 (ps <> ps2)
  mappend (Append0 ps) p = Append0 (ps <> pure p)
  mappend p (Append0 ps) = Append0 (pure p <> ps)
  mappend p p2 = Append0 (pure p <> pure p2)

instance IsString s => IsString (Pretty0 e s) where
  fromString s = Lit0 (fromString s)

external :: e -> Pretty0 e s
external = External0

fromPretty0 :: forall s e . (LL.ListLike s Char, IsString s) => (e -> Pretty s) -> Pretty0 e s -> Pretty s
fromPretty0 renderE p = case p of
  Empty0 -> mempty
  Lit0 s -> lit' (foldMap chDelta $ LL.toList s) s
  OrElse0 p1 p2 -> fromPretty0 renderE p1 `orElse'` fromPretty0 renderE p2
  Group0 p -> group' $ fromPretty0 renderE p
  External0 e -> renderE e
  Append0 ps -> foldMap (fromPretty0 renderE) ps
  Wrap0 p -> wrap' $ fromPretty0 renderE p
  HangUngrouped0 from by p ->
    hangUngrouped'
      (fromPretty0 renderE from)
      (fromPretty0 renderE by)
      (fromPretty0 renderE p)
    where
    hangUngrouped' :: Pretty s -> Pretty s -> Pretty s -> Pretty s
    hangUngrouped' from by p = if preferredHeight p > 0
      then from <> "\n" <> indent' by p
      else (from <> " " <> p) `orElse'` (from <> "\n" <> indent' by p)
  Hang0 from by p ->
    hang'
      (fromPretty0 renderE from)
      (fromPretty0 renderE by)
      (fromPretty0 renderE p)
    where
    hang' :: Pretty s -> Pretty s -> Pretty s -> Pretty s
    hang' from by p = group' $ if preferredHeight p > 0
      then from <> "\n" <> group' (indent' by p)
      else (from <> " " <> group' p) `orElse'` (from <> "\n" <> group' (indent' by p))
  IndentAfterNewline0 by p ->
    indentAfterNewline' (fromPretty0 renderE by) (fromPretty0 renderE p)
  TwoColumn0 ps -> let
    col2 rows = lines' (group' <$> alignedRows)
      where
      maxWidth = foldl' max 0 (preferredWidth . fst <$> rows) + 1
      alignedRows =
        [ rightPad maxWidth col0 <>
          fromPretty0 renderE (indentNAfterNewline maxWidth col1)
        | (col0, col1) <- rows
        ]
    in col2 [(fromPretty0 renderE l, r) | (l,r) <- ps ]

group' p = Pretty (delta p) (Group p)
lines' ps = intercalateMap "\n" id ps
indent' by p = by <> indentAfterNewline' by p
indentAfterNewline' by p = flatMap f p where
  f s0 = case LL.break (== '\n') s0 of
    (hd, s) -> if LL.null s
      then lit'' s0
      -- use `take` and `drop` to preserve annotations or
      -- or other extra info attached to the original `s`
      else lit'' (LL.take (LL.length hd) s0) <>
           "\n" <> by <> f (LL.drop 1 s)

mapLit :: (s -> t) -> F s r -> F t r
mapLit f (Lit s) = Lit (f s)
mapLit _ Empty = Empty
mapLit _ (Group r) = Group r
mapLit _ (Wrap s) = Wrap s
mapLit _ (OrElse r s) = OrElse r s
mapLit _ (Append s) = Append s

lit :: s -> Pretty0 e s
lit = Lit0

lit' :: Delta -> s -> Pretty s
lit' d s = Pretty d (Lit s)

lit'' :: (LL.ListLike s Char, IsString s) => s -> Pretty s
lit'' s = lit' (foldMap chDelta $ LL.toList s) s

orElse :: Pretty0 e s -> Pretty0 e s -> Pretty0 e s
orElse = OrElse0

orElse' :: Pretty s -> Pretty s -> Pretty s
orElse' p1 p2 = Pretty (delta p1) (OrElse p1 p2)

orElses :: [Pretty0 e s] -> Pretty0 e s
orElses [] = mempty
orElses ps = foldr1 orElse ps

wrapImpl :: IsString s => [Pretty s] -> Pretty s
wrapImpl [] = mempty
wrapImpl (p:ps) = wrap_ . Seq.fromList $
  p : fmap (\p -> (" " <> p) `orElse'` ("\n" <> p)) ps

wrapString :: (LL.ListLike s Char, IsString s) => String -> Pretty0 e s
wrapString s = wrap (lit $ fromString s)

wrap :: Pretty0 e s -> Pretty0 e s
wrap = Wrap0

wrap' :: forall s . (LL.ListLike s Char, IsString s) => Pretty s -> Pretty s
wrap' p = wrapImpl (toLeaves [p]) where
  toLeaves :: [Pretty s] -> [Pretty s]
  toLeaves [] = []
  toLeaves (hd:tl) = case out hd of
    Empty -> toLeaves tl
    Lit s -> wordify s ++ toLeaves tl
    Group _ -> hd : toLeaves tl
    OrElse a _ -> toLeaves (a:tl)
    Wrap _ -> hd : toLeaves tl
    Append hds -> toLeaves (toList hds ++ tl)
  wordify :: s -> [Pretty s]
  wordify s0 = let s = LL.dropWhile isSpace s0 in
    if LL.null s then []
    else case LL.break isSpace s of
      (word1, s) -> let
        w = lit' (foldMap chDelta $ LL.toList word1) word1
        in w : wordify s

wrap_ :: Seq (Pretty s) -> Pretty s
wrap_ ps = Pretty (foldMap delta ps) (Wrap ps)

group :: Pretty0 e s -> Pretty0 e s
group p = Group0 p

toANSI :: Width -> Pretty CT.ColorText -> String
toANSI avail p = CT.toANSI (render avail p)

toPlain :: Width -> Pretty CT.ColorText -> String
toPlain avail p = CT.toPlain (render avail p)

renderUnbroken :: (Monoid s, IsString s) => Pretty s -> s
renderUnbroken = render maxBound

render :: (Monoid s, IsString s) => Width -> Pretty s -> s
render availableWidth p = go mempty [Right p] where
  go _   []       = mempty
  go cur (p:rest) = case p of
    Right p -> -- `p` might fit, let's try it!
      if p `fits` cur then flow p <> go (cur <> delta p) rest
      else go cur (Left p : rest) -- nope, switch to breaking mode
    Left p -> case out p of -- `p` requires breaking
      Append ps  -> go cur ((Left <$> toList ps) <> rest)
      Empty      -> go cur rest
      Group p    -> go cur (Right p : rest)
      -- Note: literals can't be broken further so they're
      -- added to output unconditionally
      Lit l      -> l <> go (cur <> delta p) rest
      OrElse _ p -> go cur (Right p : rest)
      Wrap ps    -> go cur ((Right <$> toList ps) <> rest)

  flow p = case out p of
    Append ps -> foldMap flow ps
    Empty -> mempty
    Group p -> flow p
    Lit s -> s
    OrElse p _ -> flow p
    Wrap ps -> foldMap flow ps

  fits p cur =
    let cur' = cur { maxCol = col cur }
    in maxCol (cur' <> delta p) < availableWidth

newline :: IsString s => Pretty0 e s
newline = "\n"

spaceIfBreak :: IsString s => Pretty0 e s
spaceIfBreak = "" `orElse` " "

spacesIfBreak :: IsString s => Int -> Pretty0 e s
spacesIfBreak n = "" `orElse` (fromString $ replicate n ' ')

softbreak :: IsString s => Pretty0 e s
softbreak = " " `orElse` newline

spaced :: (Foldable f, IsString s) => f (Pretty0 e s) -> Pretty0 e s
spaced = intercalateMap softbreak id

spacedMap :: (Foldable f, IsString s) => (a -> Pretty0 e s) -> f a -> Pretty0 e s
spacedMap f as = spaced . fmap f $ toList as

commas :: (Foldable f, IsString s) => f (Pretty0 e s) -> Pretty0 e s
commas = intercalateMap ("," <> softbreak) id

oxfordCommas :: (Foldable f, IsString s) => f (Pretty0 e s) -> Pretty0 e s
oxfordCommas xs = case toList xs of
  []     -> ""
  [x]    -> x
  [x, y] -> x <> " and " <> y
  xs ->
    intercalateMap ("," <> softbreak) id (init xs)
      <> ","
      <> softbreak
      <> "and"
      <> softbreak
      <> last xs

parenthesizeCommas :: (Foldable f, IsString s) => f (Pretty0 e s) -> Pretty0 e s
parenthesizeCommas = surroundCommas "(" ")"

surroundCommas
  :: (Foldable f, IsString s)
  => Pretty0 e s
  -> Pretty0 e s
  -> f (Pretty0 e s)
  -> Pretty0 e s
surroundCommas start stop fs =
  group
    $  start
    <> spaceIfBreak
    <> intercalateMap ("," <> softbreak <> spaceIfBreak) id fs
    <> spaceIfBreak
    <> stop

sepSpaced :: (Foldable f, IsString s) => Pretty0 e s -> f (Pretty0 e s) -> Pretty0 e s
sepSpaced between = sep (between <> softbreak)

sep :: (Foldable f, IsString s) => Pretty0 e s -> f (Pretty0 e s) -> Pretty0 e s
sep between = intercalateMap between id

parenthesize :: IsString s => Pretty0 e s -> Pretty0 e s
parenthesize p = group $ "(" <> p <> ")"

parenthesizeIf :: IsString s => Bool -> Pretty0 e s -> Pretty0 e s
parenthesizeIf False s = s
parenthesizeIf True s = parenthesize s

lines :: (Foldable f, IsString s) => f (Pretty0 e s) -> Pretty0 e s
lines = intercalateMap newline id

linesSpaced :: (Foldable f, IsString s) => f (Pretty0 e s) -> Pretty0 e s
linesSpaced ps = lines (intersperse "" $ toList ps)

bulleted
  :: (Foldable f, LL.ListLike s Char, IsString s) => f (Pretty0 e s) -> Pretty0 e s
bulleted = intercalateMap newline (\b -> "* " <> indentAfterNewline "  " b)

dashed
  :: (Foldable f, LL.ListLike s Char, IsString s) => f (Pretty0 e s) -> Pretty0 e s
dashed = intercalateMap newline (\b -> "- " <> indentAfterNewline "  " b)

numbered
  :: (Foldable f, LL.ListLike s Char, IsString s)
  => (Int -> Pretty0 e s)
  -> f (Pretty0 e s)
  -> Pretty0 e s
numbered num ps = column2 (fmap num [1 ..] `zip` toList ps)

leftPad, rightPad :: IsString s => Int -> Pretty s -> Pretty s
leftPad n p =
  let rem = n - preferredWidth p
  in  if rem > 0 then fromString (replicate rem ' ') <> p else p
rightPad n p =
  let rem = n - preferredWidth p
  in  if rem > 0 then p <> fromString (replicate rem ' ') else p

column2 :: [(Pretty0 e s, Pretty0 e s)] -> Pretty0 e s
column2 = TwoColumn0

text :: IsString s => Text -> Pretty0 e s
text t = fromString (Text.unpack t)

hang' :: Pretty0 e s -> Pretty0 e s -> Pretty0 e s -> Pretty0 e s
hang' = Hang0

hangUngrouped' :: Pretty0 e s -> Pretty0 e s -> Pretty0 e s -> Pretty0 e s
hangUngrouped' = HangUngrouped0

hangUngrouped :: IsString s => Pretty0 e s -> Pretty0 e s -> Pretty0 e s
hangUngrouped from p = hangUngrouped' from "  " p

hang :: IsString s => Pretty0 e s -> Pretty0 e s -> Pretty0 e s
hang from p = hang' from "  " p

nest :: IsString s => Pretty0 e s -> Pretty0 e s -> Pretty0 e s
nest by = hang' "" by

indent :: (LL.ListLike s Char, IsString s) => Pretty0 e s -> Pretty0 e s -> Pretty0 e s
indent by p = by <> indentAfterNewline by p

indentN :: (LL.ListLike s Char, IsString s) => Width -> Pretty0 e s -> Pretty0 e s
indentN by = indent (fromString $ replicate by ' ')

indentNAfterNewline
  :: (LL.ListLike s Char, IsString s) => Width -> Pretty0 e s -> Pretty0 e s
indentNAfterNewline by = indentAfterNewline (fromString $ replicate by ' ')

indentAfterNewline :: Pretty0 e s -> Pretty0 e s -> Pretty0 e s
indentAfterNewline = IndentAfterNewline0

instance IsString s => IsString (Pretty s) where
  fromString s = lit' (foldMap chDelta s) (fromString s)

instance Semigroup (Pretty s) where (<>) = mappend
instance Monoid (Pretty s) where
  mempty = Pretty mempty Empty
  mappend p1 p2 = Pretty (delta p1 <> delta p2) .
    Append $ case (out p1, out p2) of
      (Append ps1, Append ps2) -> ps1 <> ps2
      (Append ps1, _) -> ps1 <> pure p2
      (_, Append ps2) -> pure p1 <> ps2
      (_,_) -> pure p1 <> pure p2

-- `Delta` represents a shape of a plain text output, equipped with a monoid for
-- combining deltas (even multi-line deltas) accurately. It's used to represent
-- the "preferred" dimensions of pretty document in order to make formatting
-- decisions about whether a doc can fit in the available width.
--
-- `line` is the difference in starting and ending line number
-- `col` is the number of characters output on the last line
-- `maxCol` is the maximum column of any line produced by the document
data Delta =
  Delta { line :: !Int, col :: !Int, maxCol :: !Int }
  deriving (Eq,Ord,Show)

instance Semigroup Delta where (<>) = mappend
instance Monoid Delta where
  mempty = Delta 0 0 0
  -- If the right delta has no linebreak (if it's line delta is 0),
  -- then we just add to the column of the left delta
  mappend (Delta l c mc) (Delta 0 c2 mc2) =
    Delta l (c + c2) (mc `max` mc2 `max` (c + c2))
  mappend (Delta l _ mc) (Delta l2 c2 mc2) = Delta (l + l2) c2 (mc `max` mc2)

chDelta :: Char -> Delta
chDelta '\n' = Delta 1 0 0
chDelta _ = Delta 0 1 1

preferredWidth :: Pretty s -> Width
preferredWidth p = col (delta p)

preferredHeight :: Pretty s -> Width
preferredHeight p = line (delta p)

black, red, green, yellow, blue, purple, cyan, white, hiBlack, hiRed, hiGreen, hiYellow, hiBlue, hiPurple, hiCyan, hiWhite, bold
  :: Pretty0 e CT.ColorText -> Pretty0 e CT.ColorText
black = fmap CT.black
red = fmap CT.red
green = fmap CT.green
yellow = fmap CT.yellow
blue = fmap CT.blue
purple = fmap CT.purple
cyan = fmap CT.cyan
white = fmap CT.white
hiBlack = fmap CT.hiBlack
hiRed = fmap CT.hiRed
hiGreen = fmap CT.hiGreen
hiYellow = fmap CT.hiYellow
hiBlue = fmap CT.hiBlue
hiPurple = fmap CT.hiPurple
hiCyan = fmap CT.hiCyan
hiWhite = fmap CT.hiWhite
bold = fmap CT.bold

instance Show s => Show (Pretty s) where
  show p = render 80 (fromPretty0 (const undefined) (metaPretty p))

metaPretty :: Show s => Pretty s -> Pretty0 e String
metaPretty p = go (0::Int) p where
  go prec p = case out p of
    Lit s -> parenthesizeIf (prec > 0) $ "Lit" `hang` lit (show s)
    Empty -> "Empty"
    Group g -> parenthesizeIf (prec > 0) $ "Group" `hang` go 1 g
    Wrap s -> parenthesizeIf (prec > 0) $ "Wrap" `hang`
      surroundCommas "[" "]" (go 1 <$> s)
    OrElse a b -> parenthesizeIf (prec > 0) $
      "OrElse" `hang` spaced [go 1 a, go 1 b]
    Append s -> surroundCommas "[" "]" (go 1 <$> s)

map :: (LL.ListLike s2 Char, IsString s2) => (s -> s2) -> Pretty s -> Pretty s2
map f p = case out p of
  Append ps -> foldMap (map f) ps
  Empty -> mempty
  Group p -> group' (map f p)
  Lit s -> lit'' (f s)
  OrElse p1 p2 -> orElse' (map f p1) (map f p2)
  Wrap p -> wrap_ (map f <$> p)

flatMap :: (s -> Pretty s2) -> Pretty s -> Pretty s2
flatMap f p = case out p of
  Append ps -> foldMap (flatMap f) ps
  Empty -> mempty
  Group p -> group' (flatMap f p)
  Lit s -> f s
  OrElse p1 p2 -> orElse' (flatMap f p1) (flatMap f p2)
  Wrap p -> wrap_ (flatMap f <$> p)
