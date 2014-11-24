-- Detect AES in ECB mode
-- In this file are a bunch of hex-encoded ciphertexts.
--
-- One of them has been encrypted with ECB.
--
-- Detect it.
-- Remember that the problem with ECB is that it is stateless and deterministic; the same 16 byte plaintext
-- block will always produce the same 16 byte ciphertext.

-- hamming distance average over 16 byte chunks for each string

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Bits as DB (xor, popCount)
import qualified Data.ByteString as BS (ByteString, empty, drop, take, zipWith)
import qualified Data.ByteString.Char8 as BC (readFile, lines)
import qualified Data.ByteString.Base16 as B16 (decode)
import qualified Data.List as DL (map, genericLength, minimumBy, zip)
import qualified Data.Ord as DO (comparing)

evalLine :: Fractional a => BS.ByteString -> a
evalLine line = realToFrac (sum hds) / DL.genericLength hds
  where
   combos = combine $ splitBytes line
   xordCombos = map (\x -> BS.zipWith DB.xor (fst x) (snd x)) combos
   hds = concat $ map (\x -> DL.map DB.popCount x) xordCombos

-- TODO: Maybe we should be breaking up into 8s?
splitBytes :: BS.ByteString -> [BS.ByteString]
splitBytes bs = split bs [] 
   where split rbs acc 
           | BS.empty == rbs = acc
           | otherwise = split (BS.drop 16 rbs) (BS.take 16 rbs : acc) 

-- Combines list contents into unique pairs -- 
-- ["A","B","C","D"] -> [("A","B") ("A","C") ("A","D") ("B","C") ("B","D") ("C","D")]
combine :: [a] -> [(a, a)]
combine xs = combine' xs []
  where 
  	pair y ys = DL.map (\s -> (y, s)) ys
  	combine' [] acc = acc
  	combine' (z:zs) acc = combine' zs (acc ++ (pair z zs))

main = do 
        file <- BC.readFile "8.txt"
        let lowestHd = DL.minimumBy (DO.comparing snd) $ hdMeans file
        putStrLn ("Line # " ++ fst lowestHd ++ "with a score of " ++ snd lowestHd)
       where 
        hdMeans fl = DL.zip [1..] $ DL.map evalLine $ BC.lines fl