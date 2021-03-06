module Crypto where

import Data.Char

import Prelude hiding (gcd)

{- 
The advantage of symmetric encryption schemes like AES is that they are efficient 
and we can encrypt data of arbitrary size. The problem is how to share the key. 
The flaw of the RSA is that it is slow and we can only encrypt data of size lower 
than the RSA modulus n, usually around 1024 bits (64 bits for this exercise!).

We usually encrypt messages with a private encryption scheme like AES-256 with 
a symmetric key k. The key k of fixed size 256 bits for example is then exchanged 
via the aymmetric RSA. 
-}

-------------------------------------------------------------------------------
-- PART 1 : asymmetric encryption

gcd :: Int -> Int -> Int
gcd m n
    | n == 0 = m
    | otherwise = gcd n (m `mod` n)

phi :: Int -> Int
phi m = length [x | x <- [1..m], (gcd x m == 1)]

--
-- Calculates (u, v, d) the gcd (d) and Bezout coefficients (u and v) 
-- such that au + bv = d
--
extendedGCD :: Int -> Int -> ((Int, Int), Int)
extendedGCD a b
    | b == 0 = ((1,0),a)
    | otherwise  = ((v',u'- (q*v')),gcd a b)
    where (q,r) = quotRem a b
          ((u',v'),d) = extendedGCD b r

-- Inverse of a modulo m
inverse :: Int -> Int -> Int
inverse a m = u `mod` m
    where ((u,v),d) = extendedGCD a m

-- Calculates (a^k mod m)
-- 
modPow :: Int -> Int -> Int -> Int
modPow a k m
    | m == 1 = 0 
    | k == 0 = 1   
    | k == 1 = a `mod` m
    | even k = modPow (a^2 `mod` m) (k `div` 2)  m
    | otherwise = (a `mod` m)*(modPow ((a `mod` m)^2 `mod` m) (k `div` 2) m) `mod`m




-- Returns the smallest integer that is coprime with phi
smallestCoPrimeOf :: Int -> Int
smallestCoPrimeOf phi = smallestCoPrimeOf' phi 2
    where smallestCoPrimeOf' a b
            | gcd a b == 1 = b
            | otherwise = smallestCoPrimeOf' a (b+1)

-- Generates keys pairs (public, private) = ((e, n), (d, n))
-- given two "large" distinct primes, p and q
genKeys :: Int -> Int -> ((Int, Int), (Int, Int))
genKeys p q = ((e,p*q),(inverse e ((p-1)*(q-1)),p*q))
    where e = smallestCoPrimeOf ((p-1)*(q-1))

-- RSA encryption/decryption; (e, n) is the public key
rsaEncrypt :: Int -> (Int, Int) -> Int
rsaEncrypt m (e, n) = modPow m e n

rsaDecrypt :: Int -> (Int, Int) -> Int
rsaDecrypt c (d,n)  = modPow c d n


-------------------------------------------------------------------------------
-- PART 2 : symmetric encryption

-- Returns position of a letter in the alphabet
toInt :: Char -> Int
toInt a = ord a - ord 'a'

-- Returns the n^th letter
toChar :: Int -> Char
toChar n = chr (n + ord 'a')

-- "adds" two letters
add :: Char -> Char -> Char
add a b = toChar ((toInt a + toInt b) `mod` ((ord 'z' - ord 'a') +1))  

-- "substracts" two letters
substract :: Char -> Char -> Char
substract a b = toChar((toInt a - toInt b) `mod` ((ord 'z' - ord 'a') +1))

-- the next functions present
-- 2 modes of operation for block ciphers : ECB and CBC
-- based on a symmetric encryption function e/d such as "add"

-- ecb (electronic codebook) with block size of a letter
--
ecbEncrypt :: Char -> String -> String
ecbEncrypt key m = map (add key) m 

ecbDecrypt :: Char -> String -> String
ecbDecrypt key [] = []
ecbDecrypt key  (m:ms) = substract m key : ecbDecrypt key ms

-- cbc (cipherblock chaining) encryption with block size of a letter
-- initialisation vector iv is a letter
-- last argument is message m as a string
--
cbcEncrypt :: Char -> Char -> String -> String
cbcEncrypt key iv [] = []
cbcEncrypt key iv (m:ms) = add key (add m iv) : cbcEncrypt key (add key (add m iv)) ms


{--cbcDecrypt :: Char -> Char -> String -> String
cbcDecrypt key iv [] = []
cbcDecrypt key iv (m:ms) 
  = (substract (substract m iv) key) : cbcDecrypt key (substract (substract m iv) key) ms

--}


cbcDecrypt :: Char -> Char -> String -> String
cbcDecrypt key iv m
   = cbcDecrypt' key iv m
   where
      cbcDecrypt' key iv m
        | null m = []
        | otherwise = x : cbcDecrypt' key (head m) xs
        where
           x = substract (substract (head m) iv) key
           xs = tail m





