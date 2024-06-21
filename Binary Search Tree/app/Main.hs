module Main where

import Dictionary as Dict

main :: IO ()
main = do
  -- Create an empty dictionary
  let dict = Dict.empty

  -- Insert Key Value Pairs
  let dict' = Dict.insert (1, "Hello") $
              Dict.insert (4, "OpenAI") $
              Dict.insert (2, "World") $
              Dict.insert (6, "ChatGPT") $
              Dict.insert (5, "GPT") $
              Dict.insert (3, "Test") dict

  -- Lookup Key Value Pair
  putStrLn $ "Lookup 2: " ++ show (Dict.lookup 2 dict')
  putStrLn $ "Lookup 5: " ++ show (Dict.lookup 5 dict')

  -- Print Ordered List of all entries in the dictionary
  putStrLn "Ordered List of all entries in the dictionary:"
  print (Dict.toList dict')

  -- Remove an entry by key
  let dict'' = Dict.remove 2 dict'  -- Removing key '2'
  putStrLn "Ordered List of all entries after removing key 2:"
  print (Dict.toList dict'')

  -- Test removeIf function by removing odd keys
  let dictOddRemoved = Dict.removeIf (\k -> k `mod` 2 /= 0) dict''
  putStrLn "Ordered List of all entries after removing odd keys:"
  print (Dict.toList dictOddRemoved)