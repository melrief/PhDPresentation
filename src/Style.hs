{-# LANGUAGE OverloadedStrings #-}
module Style where

import Text.LaTeX

import Commands

import Style.Outer (outerStyle)


presentationStyle :: Monad m => LaTeXT_ m
presentationStyle = do

  mode "presentation"

  useinnertheme ["shadow=true"] "rounded"

  outerStyle

  usecolortheme "beaver"

  setbeamerfont "block title" (raw "size={}")

  setbeamercolor "titlelike" "parent=structure,bg=white"

  mode "all"

  -- bullets style
  setbeamertemplate "itemize item" (usebeamercolor "fg" "title" <> large mvRightarrow)
  setbeamertemplate "itemize subitem" (usebeamercolor "fg" "title" <> flatsteel)
  setbeamertemplate "itemize subsubitem" (usebeamercolor "bg" "frametitle right" <> flatsteel)

  -- caption without stuff at the beginning
  setbeamertemplate "caption" (raggedright <> scriptsize insertcaption <> par)

  -- reduce the caption distance from the figure
  setlength abovecaptionskip (Pt 2)
