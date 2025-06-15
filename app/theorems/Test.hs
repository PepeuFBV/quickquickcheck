module Test
  ( plusOne,
  )
where

-- @quickcheck plusOne(x) == x + 1
-- @quickcheck plusOne(0) == 1
-- @quickcheck plusOne(1) == 2
plusOne :: Integer -> Integer
plusOne x = x + 1