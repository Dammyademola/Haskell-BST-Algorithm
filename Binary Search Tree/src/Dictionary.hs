module Dictionary
  ( Dictionary, empty, insert, lookup, toList, remove, removeIf)
  where

import Prelude hiding (lookup)
import qualified BinarySearchTree as BST

type Dictionary = BST.BST

empty :: Dictionary
empty = BST.Leaf

insert :: (Int, String) -> Dictionary -> Dictionary
insert = BST.insert

lookup :: Int -> Dictionary -> Maybe String
lookup = BST.bstLookup

toList :: Dictionary -> [(Int, String)]
toList = BST.toList

remove :: Int -> Dictionary -> Dictionary
remove = BST.remove

removeIf :: (Int -> Bool) -> Dictionary -> Dictionary
removeIf = BST.removeIf