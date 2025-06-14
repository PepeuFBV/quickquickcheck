module Test
  ( plusOne,
  )
where

-- @quickcheck plusOne(x) == x + 1
plusOne :: Integer -> Integer
plusOne x = x + 1