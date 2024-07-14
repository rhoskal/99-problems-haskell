module BinaryTree
  ( BinaryTree (..),
    depth,
    prettyPrint,
  )
where

data BinaryTree a
  = Empty
  | Branch a (BinaryTree a) (BinaryTree a)
  deriving (Eq, Show)

depth :: BinaryTree a -> Int
depth Empty = 0
depth (Branch _ l r) = 1 + max (depth l) (depth r)

{- Output:
 ( Branch
     1
     (Branch 12 (Branch 122 Empty Empty) (Branch 121 Empty Empty))
     (Branch 11 (Branch 112 Empty Empty) (Branch 111 Empty Empty))
 )

 1
 +- 11
 |  +- 111
 |  `- 112
 `- 12
    +- 121
    `- 122

 Credit to Raekye -> https://stackoverflow.com/questions/12556469/nicely-printing-showing-a-binary-tree-in-haskell
-}
prettyPrint :: (Show a) => BinaryTree a -> String
prettyPrint Empty = "Empty root."
prettyPrint tree = unlines (prettyPrint' tree)

prettyPrint' :: (Show a) => BinaryTree a -> [String]
prettyPrint' Empty = []
prettyPrint' (Branch a left right) = show a : go left right
  where
    go :: (Show a) => BinaryTree a -> BinaryTree a -> [String]
    go l r = pad "+- " "|  " (prettyPrint' r) ++ pad "`- " "   " (prettyPrint' l)

    pad :: String -> String -> [String] -> [String]
    pad first rest = zipWith (++) (first : repeat rest)

-- https://apfelmus.nfshost.com/articles/monoid-fingertree.html
-- https://www.anardil.net/2018/binary-tree-in-haskell.html
-- https://rlgomes.github.io/work/haskell/2011/10/31/20.00-Custom-data-types-in-Haskell.html
-- https://tylerreckart.gitbooks.io/haskell/content/notes/learn_you_a_haskell/07-customTypes.html
