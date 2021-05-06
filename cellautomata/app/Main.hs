module Main where

import Universe
import Data.Word
import Data.Bits
import MyGraphics
import Graphics.Gloss

main = do
  putStrLn "Введите номер правила (0-255)\n"
  x <- getLine
  let rulenum = read x :: Word8 
  simulate windowdisplay white fps initialmodel (drawingfunc rulenum) (updatefunc rulenum)
