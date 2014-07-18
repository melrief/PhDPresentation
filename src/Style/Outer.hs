{-# LANGUAGE OverloadedStrings #-}
module Style.Outer where

import Text.LaTeX

import Commands


outerStyle :: Monad m => LaTeXT_ m
outerStyle = do

  mode "presentation"

  setbeamercolor_ "author in head/foot" "parent=palette tertiary"
  setbeamercolor_ "title in head/foot" "parent=palette secondary"
  setbeamercolor_ "date in head/foot" "parent=palette primary"
  setbeamercolor_ "section in head/foot" "parent=palette tertiary"
  setbeamercolor_ "sebsection in head/foot" "parent=palette primary"

  let boxesOptions p = ["wd=0.333333" <> paperwidth,"ht=" <> rendertex (Ex 2.25),"dp=" <> rendertex (Ex 1),p]
  defbeamertemplate_ "footline" "custom theme" $ do
    leavevmode
    hbox $ do
      beamercolorbox (boxesOptions "center") "author in head/foot" $ do
        usebeamerfont "author in head/foot"
        insertshortauthor <> expandafter <> hspace (Pt 1) <> "(" <> insertshortinstitute <> ")"
      beamercolorbox (boxesOptions "center") "title in head/foot" $ do
        usebeamerfont "title in head/foot"
        insertshorttitle
      beamercolorbox (boxesOptions "right") "date in head/foot" $ do
        usebeamerfont "date in head/foot"
        insertshortdate <> hspace_ (Em 2) <> insertframenumber <> hspace_ (Em 2)
    vskip $ Pt 0
  
  defbeamertemplate_ "headline" "custom theme" (vskip $ Pt 0)

  setbeamersize [("text margin left",Em 1),("text margin right",Em 1)]

  mode "all"
