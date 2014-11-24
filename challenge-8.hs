-- Detect AES in ECB mode
-- In this file are a bunch of hex-encoded ciphertexts.
--
-- One of them has been encrypted with ECB.
--
-- Detect it.
-- Remember that the problem with ECB is that it is stateless and deterministic; the same 16 byte plaintext
-- block will always produce the same 16 byte ciphertext.

-- (133, 3.3305555555555557)

{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import qualified Data.Bits              as DB (popCount, xor)
import qualified Data.ByteString        as BS (ByteString, drop, empty, take,
                                               zipWith)
import qualified Data.ByteString.Base16 as B16 (decode)
import qualified Data.ByteString.Char8  as BC (lines, readFile)
import qualified Data.List              as DL (genericLength, map, minimumBy,
                                               zip)
import qualified Data.Ord               as DO (comparing)

calcHDMean :: BS.ByteString -> Double
calcHDMean line = realToFrac (sum hds) / DL.genericLength hds
  where
   combos = combine $ splitBytes line
   xordCombos = map (uncurry $ BS.zipWith DB.xor) combos
   hds = concatMap (DL.map DB.popCount) xordCombos

splitBytes :: BS.ByteString -> [BS.ByteString]
splitBytes bs = split bs []
   where split bs acc
           | BS.empty == bs = acc
           | otherwise = split (BS.drop 16 bs) (BS.take 16 bs : acc)

-- Combines list contents into unique pairs --
-- ["A","B","C","D"] -> [("A","B") ("A","C") ("A","D") ("B","C") ("B","D") ("C","D")]
combine :: [a] -> [(a, a)]
combine xs = combine' xs []
  where
    pair y = DL.map (\s -> (y, s))
    combine' [] acc = acc
    combine' (z:zs) acc = combine' zs (acc ++ pair z zs)

-- Test HD. Should be 37.
-- hd bs1 bs2 = BS.zipWith DB.xor bs1 bs2
-- hdTest = DL.map DB.popCount $ hd (BC.pack "this is a test") (BC.pack "wokka wokka!!!")
-- sum hdTest

main :: IO ()
main = do
    bytes <- DL.map (fst . B16.decode) <$> BC.lines <$> BC.readFile "8.txt"
    let hdMeans = DL.map calcHDMean bytes
    let lowestHd = DL.minimumBy (DO.comparing snd) $ DL.zip [1..] hdMeans
    putStrLn $ "Line # " ++ show (fst lowestHd) ++ " with a score of " ++ show (snd lowestHd)
