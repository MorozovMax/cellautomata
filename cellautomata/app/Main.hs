module Main where

import Universe
import Data.Word
import Data.Bits
import MyGraphics
import Graphics.Gloss
import Text.Read

main = do
  putStrLn "Введите номер правила (0-255)\n"
  x <- getLine
  let rulenum = readMaybe x :: Maybe Word8
  case rulenum of
  	Just rulenum -> simulate windowdisplay white fps initialmodel (drawingfunc rulenum) (updatefunc rulenum)
	otherwise -> putStrLn "Wrong input!"

