module Common (foldlDigits) where

foldlDigits :: Integral b => (a -> b -> a) -> a -> b -> a
foldlDigits f a n
  | n < 0     = foldlDigits f a (-n)
  | n < 10    = f a n
  | otherwise = f (foldlDigits f a (n `div` 10)) (n `mod` 10)
