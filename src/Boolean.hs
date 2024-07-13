module Boolean
  ( and',
    not',
    or',
    nand',
    nor',
    xor',
    equ',
  )
where

and' :: Bool -> Bool -> Bool
and' a b = a && b

or' :: Bool -> Bool -> Bool
or' a b = a || b

not' :: Bool -> Bool
not' = not

nand' :: Bool -> Bool -> Bool
nand' a b = not' (and' a b)

nor' :: Bool -> Bool -> Bool
nor' a b = not' (or' a b)

xor' :: Bool -> Bool -> Bool
xor' a b = not' (equ' a b)

equ' :: Bool -> Bool -> Bool
equ' a b = a == b
