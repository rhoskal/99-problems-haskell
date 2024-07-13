module BinaryTree (BinaryTree (..)) where

data BinaryTree a
  = Empty
  | Branch a (BinaryTree a) (BinaryTree a)
  deriving (Eq, Show)

