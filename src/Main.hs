module Main where

import System.Environment
  (getArgs
  ,getProgName)
import qualified System.FilePath as FP
import Text.LaTeX.Base.Pretty
import Text.LaTeX.Base.Render (renderFile)
import Text.LaTeX.Base.Writer 

import Presentation
import Style


main :: IO ()
main = getArgs >>= main'

-- Create the presentation tex file and the style file
main' :: [String] -> IO ()
main' []          = getProgName >>= \pn -> putStrLn $ "Usage: " ++ pn ++ " <file_to_write>"
main' (outFile:_) = do

  let baseFileName      = FP.dropExtension outFile
  let prettyFile        = FP.addExtension (baseFileName ++ "Pretty") "tex"
  let baseStyleFileName = baseFileName ++ "Style"
  let styleFile         = FP.addExtension baseStyleFileName "sty"
  let stylePrettyFile   = FP.addExtension (baseStyleFileName ++ "Pretty") "sty"


  -- HaTeX shows the produced tex code in one line by default. There is also
  -- the possibility to pretty print it but this feature is unstable. Therefore
  -- we print two tex files, one "one-line" and the other more readable (*pretty file)

  -- make the presentation (we need to pass the style file without extension
  -- because the extension is automatically added by Latex that is not smart
  -- enough to understand that it should add .sty to a path that ends already with sty)
  pres <- execLaTeXT $ presentation $ FP.dropExtension styleFile
  
  renderFile outFile pres
  writeFile prettyFile (prettyLaTeX pres)

  
  -- creates the style file
  style <- execLaTeXT presentationStyle

  renderFile styleFile style
  writeFile stylePrettyFile (prettyLaTeX style)
