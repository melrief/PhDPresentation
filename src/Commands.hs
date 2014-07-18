{-# LANGUAGE OverloadedStrings #-}
module Commands where

import Text.LaTeX.Base
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Syntax


-- shortcut to remove navigation symbols without using the latex name...
nonavigationsymbols :: LaTeXC l => l
nonavigationsymbols = commS "beamertemplatenavigationsymbolsempty" -- lol latex code

usebeamercolor :: LaTeXC l => LaTeX -> l -> l
usebeamercolor x = liftL $ \y -> TeXComm "usebeamercolor" [OptArg x,FixArg y]

usebeamerfont :: LaTeXC l => l -> l
usebeamerfont = liftL $ \y -> TeXComm "usebeamerfont" [FixArg y]

useinnertheme :: LaTeXC l => [LaTeX] -> l -> l
useinnertheme os = liftL $ \x -> TeXComm "useinnertheme" [MOptArg os,FixArg x]

usecolortheme :: LaTeXC l => l -> l
usecolortheme = liftL $ \c -> TeXComm "usecolortheme" [FixArg c]

setbeamerfont :: LaTeXC l => l -> l -> l
setbeamerfont = liftL2 $ \c v -> TeXComm "setbeamerfont" [FixArg c,FixArg v]

setbeamercolor :: LaTeXC l => l -> l -> l
setbeamercolor = liftL2 $ \n v -> TeXComm "setbeamercolor" [FixArg n,FixArg v]

setbeamercolor_ :: LaTeXC l => l -> l -> l
setbeamercolor_ = liftL2 $ \n v -> TeXComm "setbeamercolor*" [FixArg n,FixArg v]

setbeamertemplate :: LaTeXC l => l -> l -> l
setbeamertemplate = liftL2 $ \s t -> TeXComm "setbeamertemplate" [FixArg s,FixArg t]

defbeamertemplate_ :: LaTeXC l => l -> l -> l -> l
defbeamertemplate_ = liftL3 $ \s tn c -> TeXComm "defbeamertemplate*" [FixArg s,FixArg tn,FixArg c]

setbeamersize :: LaTeXC l => [(LaTeX,Measure)] -> l
setbeamersize ls = fromLaTeX $ TeXComm "setbeamersize" [FixArg $ rendertex ls']
  where ls' = intercalate "," $ fmap renderTuple ls
        intercalate _ [] = ""
        intercalate _ (x:[]) = x
        intercalate c (x:xs) = x <> c <> intercalate c xs
        renderTuple (k,v) = k <> "=" <> rendertex v

shortstack :: LaTeXC l => LaTeX -> l -> l
shortstack pos = liftL $ \vals -> TeXComm "shortstack" [OptArg pos,FixArg vals]

-- | Cell taking multiple rows.
multirow :: LaTeXC l => Int -> l -> l -> l
multirow n = liftL2 $ \l1 l2 -> TeXComm "multirow" [ FixArg $ rendertex n,FixArg l1,FixArg l2]

raggedright :: LaTeXC l => l
raggedright = commS "raggedright"

insertcaption :: LaTeXC l => l
insertcaption = commS "insertcaption"

mvRightarrow :: LaTeXC l => l
mvRightarrow = commS "MVRightarrow"

flatsteel :: LaTeXC l => l
flatsteel = commS "Flatsteel"

rightarrow :: LaTeXC l => l
rightarrow = commS "Rightarrow"

abovecaptionskip :: LaTeXC l => l
abovecaptionskip = commS "abovecaptionskip"

setlength :: LaTeXC l => LaTeX -> Measure -> l
setlength x y = fromLaTeX $ TeXComm "setlength" [FixArg x,FixArg $ texy y]

-- creates a hypersetup from a list of key-val pairs
-- TODO: should be a Map but LaTeX isn't an instance of Ord
hypersetup :: LaTeXC l => [(LaTeX,LaTeX)] -> l
hypersetup = hypersetup' . intercalate "," . fmap toKeyVal
  where toKeyVal (k,v) = rendertex k <> raw "={" <> rendertex v <> raw "}"
        hypersetup' = liftL $ \c -> TeXComm "hypersetup" [FixArg c]
        intercalate _ []     = ""
        intercalate _ (x:[]) = x
        intercalate c (x:xs) = x <> c <> intercalate c xs

leavevmode :: LaTeXC l => l
leavevmode = comm0 "leavevmode"

beamercolorbox :: LaTeXC l => [LaTeX] -> l -> l -> l
beamercolorbox os = liftL2 $ \x -> TeXEnv "beamercolorbox" [MOptArg os,FixArg x]

insertshortauthor :: LaTeXC l => l
insertshortauthor = commS "insertshortauthor"

insertshortinstitute :: LaTeXC l => l
insertshortinstitute = commS "insertshortinstitute"

insertshorttitle :: LaTeXC l => l
insertshorttitle = commS "insertshorttitle"

insertframenumber :: LaTeXC l => l
insertframenumber = commS "insertframenumber"

insertshortdate :: LaTeXC l => l
insertshortdate = comm0 "insertshortdate"

expandafter :: LaTeXC l => l
expandafter = comm0 "expandafter"

vskip :: LaTeXC l => Measure -> l
vskip m = commS "vskip" <> rendertex m

hbox :: LaTeXC l => l -> l
hbox = liftL $ \x -> TeXComm "hbox" [FixArg x]

paperwidth :: LaTeXC l => l
paperwidth = comm0 "paperwidth"

totalheight :: LaTeXC l => l
totalheight = comm0 "totalheight"

mode :: LaTeXC l => l -> l
mode = liftL $ \x -> TeXComm "mode" [SymArg x]

rowspacing :: LaTeXC l => Measure -> l
rowspacing m = raw "\\\\[" <> rendertex m <> "]"
