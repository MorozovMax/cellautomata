module MyGraphics where

import Graphics.Gloss
import Data.Word
import Universe
import Data.Bits.Bitwise (fromListBE)
import Data.ByteString (ByteString, pack, unpack)

windowdisplay :: Display
windowdisplay = InWindow "Window" (1001, 1000) (700, 300)

rule :: [Word8]
rule = [0..255]

bitmapData :: Word8 -> ByteString
bitmapData xs = pack $ concat $ generations
    where
      generations = map showGen $ run (wolframRule xs) wolframWorld 1000 500
      showGen cs = concat $ map showCell cs

myPicture :: Word8 -> Picture
myPicture xs = bitmapOfByteString 1001 1000 (BitmapFormat TopToBottom PxRGBA) (bitmapData xs) True

