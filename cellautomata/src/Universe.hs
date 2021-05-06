module Universe where

import Data.Bits
import Data.Bits.Bitwise (fromListBE)
import Data.Word

data Universe a = Universe [a] a [a]

instance Functor Universe where
    fmap f (Universe as x bs) = Universe (fmap f as) (f x) (fmap f bs)

extract :: Universe a -> a
extract (Universe _ x _ ) = x

double :: Universe a -> Universe (Universe a)
double w = Universe (tail $ iterate left w) w (tail $ iterate right w)

left, right :: Universe a -> Universe a
left  (Universe (l:ls) x  rs ) = Universe ls l (x:rs)
right (Universe ls x (r:rs)) = Universe (x:ls) r rs

evolve :: (Universe a -> a) -> Universe a -> Universe a
evolve rule w = fmap rule $ double w

evolveN :: (Universe a -> a) -> Universe a -> Int -> [ Universe a ]
evolveN rule w n = take n $ iterate (evolve rule) w

run :: (Universe a -> a) -> Universe a -> Int -> Int -> [[a]]
run rule w n d = map (list . (truncateD d)) $ evolveN rule w n
    where
        list (Universe ls x rs) = reverse ls ++ [x] ++ rs
        truncateD d (Universe ls x rs) = Universe (take d ls) x (take d rs)

wolframWorld :: Universe Bool
wolframWorld = Universe (repeat False) True (repeat False)

wolframRule :: Word8 -> Universe Bool -> Bool
wolframRule x w = testBit x $ fromListBE (fmap extract [left w, w, right w])

showCell :: Bool -> [Word8]
showCell True  = [0, 0, 0, 255]
showCell False = [255, 255, 255, 255]
