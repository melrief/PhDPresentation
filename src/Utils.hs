{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils where

import Text.LaTeX.Base hiding (item)
import qualified Text.LaTeX.Base.Commands as Commands
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.Graphicx


l,c,r :: TableSpec
l = LeftColumn
c = CenterColumn
r = RightColumn

-- (p)ar (m)iddle
pm :: Float -> TableSpec
pm = ParColumnMid . (<> textwidth) . rendertex

-- -|- is an operator for tables that adds a vertical line between two table specs
class    Vl v           where (-|-) :: TableSpec -> v -> [TableSpec]
infixr 8 -|-
instance Vl TableSpec   where ts1 -|- ts2 = [ts1,VerticalLine,ts2]
instance Vl [TableSpec] where ts1 -|- tss = ts1 : VerticalLine : tss

inthemiddle :: LaTeXC l => l -> l
inthemiddle w = vfill <> center w <> vfill

titleinthemiddle :: LaTeXC l => l -> l
titleinthemiddle = inthemiddle . large3 . textbf

-- concatenation with space in the middle (TODO: find better symbols)
(<.>) :: LaTeXC l => l -> l -> l
b <.> a = b <> " " <> a

item :: LaTeXC l => l -> l
item x = Commands.item Nothing <> x

tabular' :: LaTeXC l => [TableSpec] -> l -> l
tabular' = tabular Nothing

figure' :: LaTeXC l => l -> l
figure' = figure Nothing

customwidth :: Float -> IGOption
customwidth = IGWidth . CustomMeasure . (<> textwidth) . rendertex
