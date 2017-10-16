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
    | isAlpha y = (seps', ((y:w):ws))
    | isWS y = (y:seps', "" : (w:ws))
    | elem y seps = (y:seps',"" : (w:ws))
    where
      (seps', (w:ws)) = split seps ys

--isAlpha :: Char -> Bool
--isAlpha a = (ord 'a' <= a && a <= ord 'z') || (ord 'A' <= a && a <= ord 'Z') 


isWS :: Char -> Bool
isWS a = a == ' '


combine :: String -> [String] -> [String]
combine = error "TODO: implement combine"

getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs = error "TODO: implement getKeywordDefs"

expand :: FileContents -> FileContents -> FileContents
expand = error "TODO: implement expand"

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

