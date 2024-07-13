module BinaryTree
  ( BinaryTree (..),
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

prettyPrint :: BinaryTree a -> IO ()
prettyPrint Empty = undefined
prettyPrint (Branch _ _ _) = undefined

-- https://apfelmus.nfshost.com/articles/monoid-fingertree.html
-- https://stackoverflow.com/questions/12556469/nicely-printing-showing-a-binary-tree-in-haskell
-- https://www.anardil.net/2018/binary-tree-in-haskell.html
-- https://rlgomes.github.io/work/haskell/2011/10/31/20.00-Custom-data-types-in-Haskell.html
-- https://tylerreckart.gitbooks.io/haskell/content/notes/learn_you_a_haskell/07-customTypes.html
