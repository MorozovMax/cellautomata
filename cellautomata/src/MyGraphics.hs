module MyGraphics where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Data.Word
import Universe
import Data.Bits.Bitwise (fromListBE)
import Data.ByteString (ByteString, pack, unpack)

windowdisplay :: Display
windowdisplay = InWindow "Window" (501, 500) (0, 0)

rule :: [Word8]
rule = [0..255]

bitmapData :: Word8 -> Universe Bool -> ByteString
bitmapData xs model = pack $ concat $ generations
    where
      generations = map showGen $ run (wolframRule xs) model 500 250
      showGen cs = concat $ map showCell cs

myPicture :: Word8 -> Universe Bool -> Picture
myPicture xs model = bitmapOfByteString 501 500 (BitmapFormat TopToBottom PxRGBA) (bitmapData xs model) True

fps :: Int
fps = 15

initialmodel :: Universe Bool
initialmodel = Universe (repeat False) True (repeat False)

drawingfunc :: Word8 -> Universe Bool -> Picture
drawingfunc xs model = myPicture xs model

updatefunc :: Word8 -> ViewPort -> Float -> Universe Bool -> Universe Bool
updatefunc rulenum _ _ initialmodel = evolve (wolframRule rulenum) initialmodel
