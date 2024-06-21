import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Data.List (nub, sort, nubBy)

import Dictionary as Dict
import BinarySearchTree as BST

instance Arbitrary BST where
    arbitrary = sized bstGen

bstGen :: Int -> Gen BST
bstGen 0 = return Leaf
bstGen n = do
    k <- arbitrary
    v <- arbitraryString
    left <- bstGen (n `div` 2)
    right <- bstGen (n `div` 2)
    return $ InternalNode k v left right

arbitraryString :: Gen String
arbitraryString = listOf arbitraryASCIIChar

-- HUnit Tests
-- Test Dictionary Initialization
testCreateEmptyDictionary :: TestTree
testCreateEmptyDictionary = testCase "Empty dictionary should be empty" $ do
    let emptyDict = Dict.empty
    assertEqual "Check if dictionary is empty" [] (Dict.toList emptyDict)

-- Test Successful Insert And Lookup
testInsertAndLookup :: TestTree
testInsertAndLookup = testCase "Lookup existing key" $ do
    let dict = Dict.insert (1, "Hello") Dict.empty
    assertEqual "Check for inserted key" (Just "Hello") (Dict.lookup 1 dict)

-- Test Lookup Of Non-existing Key
testLookupNonExisting :: TestTree
testLookupNonExisting = testCase "Lookup non-existing key" $ do
    let dict = Dict.insert (1, "Hello") Dict.empty
    assertEqual "Non-existing key should return Nothing" Nothing (Dict.lookup 2 dict)

-- Test Ordered Displaying Of Entries
testToList :: TestTree
testToList = testCase "Ordered list of entries" $ do
    let dict = Dict.insert (2, "World") $
               Dict.insert (1, "Hello") Dict.empty
    assertEqual "List should be ordered by keys" [(1, "Hello"), (2, "World")] (Dict.toList dict)

-- Test Removal Of Specific key
testRemove :: TestTree
testRemove = testCase "Lookup key after removal" $ do
    let dict = Dict.insert (1, "Hello") $
               Dict.insert (2, "World") Dict.empty
    let removedDict = Dict.remove 1 dict
    assertEqual "Removed key should not be found" Nothing (Dict.lookup 1 removedDict)
    assertEqual "Other key should still exist" (Just "World") (Dict.lookup 2 removedDict)

-- Test Remove Entries Based On Predicate
testRemoveIf :: TestTree
testRemoveIf = testCase "Dictionary after removing keys greater than 1" $ do
    let dict = Dict.insert (3, "Test") $
               Dict.insert (1, "Hello") Dict.empty
    let modifiedDict = Dict.removeIf (> 1) dict
    assertEqual "Should only contain keys <= 1" [(1, "Hello")] (Dict.toList modifiedDict)

-- Test Rotate Tree Left Function
testRotateLeft :: TestTree
testRotateLeft = testCase "Rotate left should rotate the node correctly" $ do
    let rightChild = BST.InternalNode 3 "C" BST.Leaf BST.Leaf
    let leftChild = BST.InternalNode 1 "A" BST.Leaf BST.Leaf
    let root = BST.InternalNode 2 "B" leftChild rightChild
    let (rotatedTree, _) = BST.rotateLeft root
    let expectedTree = BST.InternalNode 3 "C" (BST.InternalNode 2 "B" leftChild BST.Leaf) BST.Leaf
    assertEqual "" expectedTree rotatedTree

-- Test Rotate Tree Right Function
testRotateRight :: TestTree
testRotateRight = testCase "Rotate right should rotate the node correctly" $ do
    let leftChild = BST.InternalNode 1 "A" BST.Leaf BST.Leaf
    let rightChild = BST.InternalNode 3 "C" BST.Leaf BST.Leaf
    let root = BST.InternalNode 2 "B" leftChild rightChild
    let (rotatedTree, _) = BST.rotateRight root
    let expectedTree = BST.InternalNode 1 "A" BST.Leaf (BST.InternalNode 2 "B" BST.Leaf rightChild)
    assertEqual "" expectedTree rotatedTree

-- Test Rebalance Tree Function
testRebalance :: TestTree
testRebalance = testCase "Rebalancing should produce a balanced tree" $ do
    let tree = BST.InternalNode 2 "B" (BST.InternalNode 1 "A" BST.Leaf BST.Leaf) (BST.InternalNode 4 "D" BST.Leaf BST.Leaf)
    let unbalancedTree = BST.InternalNode 1 "A" BST.Leaf (BST.InternalNode 2 "B" BST.Leaf (BST.InternalNode 4 "D" BST.Leaf BST.Leaf))
    let (balancedTree, _) = BST.rebalance unbalancedTree (BST.balanceFactor unbalancedTree)
    assertEqual "" tree balancedTree

-- QuickCheck Properties

-- Property Based Testing for Dictionary Insertition
prop_dictInsertLookup :: Int -> String -> Bool
prop_dictInsertLookup key val =
    let dict = Dict.insert (key, val) Dict.empty
    in Dict.lookup key dict == Just val

prop_noDuplicateKeys :: [(Int, String)] -> Bool
prop_noDuplicateKeys pairs =
    let dict = foldr (\(k, v) acc -> Dict.insert (k, v) acc) Dict.empty pairs
        uniqueKeys = nub (map fst pairs) -- Extract unique keys from input pairs
    in length (Dict.toList dict) == length uniqueKeys

prop_keysStayOrdered :: [Int] -> Bool
prop_keysStayOrdered keys =
    let sortedKeys = sort keys
        dict = foldr (\k acc -> Dict.insert (k, show k) acc) Dict.empty sortedKeys
        keysInOrder = map fst (Dict.toList dict)
    in zip keysInOrder sortedKeys == zip keysInOrder sortedKeys

prop_removeSuccessful :: Int -> Dictionary -> Bool
prop_removeSuccessful key dict =
    let dictWithKey = Dict.insert (key, "value") dict
        dictRemoved = Dict.remove key dictWithKey
    in Dict.lookup key dictRemoved == Nothing

prop_removeNotPresent :: Int -> [(Int, String)] -> Property
prop_removeNotPresent key dictList =
    let dict = foldr Dict.insert Dict.empty dictList
        keys = map fst dictList
    in key `notElem` keys ==> Dict.lookup key (Dict.remove key dict) == Nothing

-- Checks if a tree is a binary search tree
isBST :: BST -> Bool
isBST Leaf = True
isBST (InternalNode k _ left right) =
    all (< k) (keys left) && all (> k) (keys right) && isBST left && isBST right

-- Helper function to collect all keys in a BST
keys :: BST -> [Int]
keys Leaf = []
keys (InternalNode k _ left right) = keys left ++ [k] ++ keys right

-- Property to ensure no elements are lost or added during rotations
prop_rotateElementPreservation :: BST -> Bool
prop_rotateElementPreservation tree =
  sort (BST.toList tree) == sort (BST.toList (fst (rotateLeft tree))) &&
  sort (BST.toList tree) == sort (BST.toList (fst (rotateRight tree)))

-- Main Test Suite Setup
main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests"
    [ testGroup "HUnit Tests"
        [ testCreateEmptyDictionary
        , testInsertAndLookup
        , testLookupNonExisting
        , testToList
        , testRemove
        , testRemoveIf
        , testRotateLeft
        , testRotateRight
        , testRebalance
        ]
    , testGroup "QuickCheck Properties"
        [ testProperty "Insert-Lookup Consistency" prop_dictInsertLookup
        , testProperty "No Duplicate Keys on Insert" prop_noDuplicateKeys
        , testProperty "Keys Stay Ordered" prop_keysStayOrdered
        , testProperty "Successful Key Removal" prop_removeSuccessful
        , testProperty "Remove Not Present" prop_removeNotPresent
        , testProperty "Element Preservation" prop_rotateElementPreservation
        ]
    ]
