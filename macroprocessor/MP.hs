module MP where

import System.Environment
import Data.Char

type FileContents = String

type Keyword      = String
type KeywordValue = String
type KeywordDefs  = [(Keyword, KeywordValue)]

separators :: String
separators
  = " \n\t.,:;!\"\'()<>/\\"


lookUp :: String -> [(String, a)] -> [a]
lookUp str table  = [ y | (str',y) <- table, str == str']    


{--split :: [Char] -> String -> (String, [String])
split seps str = split' seps str [] [] []
      where
        split' seps' [] sepsacc wordacc words = (sepsacc,words)
        split' seps' (x:xs) sepsacc wordacc words
          | isAlpha x = split' seps' xs sepsacc (x:wordacc) words
          | isWS x || elem x seps' = split' seps' xs (x:sepsacc) [] (wordacc:words)
--}

split :: [Char] -> String -> (String, [String])
split seps [] = ("",[""])
split seps (y:ys)
    | elem y seps = (y:seps',"" : (w:ws))
    | otherwise = (seps', ((y:w):ws))
    where
      (seps', (w:ws)) = split seps ys

--isAlpha :: Char -> Bool
--isAlpha a = (ord 'a' <= a && a <= ord 'z') || (ord 'A' <= a && a <= ord 'Z') 


combine :: String -> [String] -> [String]
combine "" words = words
combine (x:xs) (y:ys) =  y: [x] : combine xs ys

getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs [] = []
getKeywordDefs (w:ws) = (y,concat (combine xs ys)) : getKeywordDefs ws
    where ((x:xs),(y:ys)) = split separators w
                        
replaceWord :: Keyword -> KeywordDefs -> KeywordValue
replaceWord key defs = head (lookUp key defs)
               
expand' :: [String] -> KeywordDefs -> [String]
expand' [] defs = []
expand' (x:xs) defs
    | x /= "" && head x == '$' = (replaceWord x defs) : (expand' xs defs)
    | otherwise = x : (expand' xs defs)

expand :: FileContents -> FileContents -> FileContents
expand words info = concat (combine seps (expand' words' (getKeywordDefs keywords))) 
    where (seps,words') = split separators words
          (_,keywords) = split "\n" info


-- You may wish to uncomment and implement this helper function
-- when implementing expand
-- replaceWord :: String -> KeywordDefs -> String



main :: IO ()
-- The provided main program which uses your functions to merge a
-- template and source file.
main = do
  args <- getArgs
  main' args

  where
    main' :: [String] -> IO ()
    main' [template, source, output] = do
      t <- readFile template
      i <- readFile source
      writeFile output (expand t i)
    main' _ = putStrLn ("Usage: runghc MP <template> <info> <output>")

