module Main where

import Universe
import Data.Word
import Data.Bits
import MyGraphics
import Graphics.Gloss

main = do
  x <- getLine
  let rulenum = read x :: Word8 
  display windowdisplay white (myPicture rulenum)
