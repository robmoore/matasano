-- Detect AES in ECB mode
-- In this file are a bunch of hex-encoded ciphertexts.
--
-- One of them has been encrypted with ECB.
--
-- Detect it.
-- Remember that the problem with ECB is that it is stateless and deterministic; the same 16 byte plaintext
-- block will always produce the same 16 byte ciphertext.

import           Control.Applicative

import qualified Data.List           as DL (maximumBy)
import qualified Data.Ord            as DO (comparing)

numEqual :: String -> Int
numEqual line = length areEqual
  where
   combos = combine $ splitString line
   boolCombos = map (uncurry (==)) combos
   areEqual = filter id boolCombos

splitString :: String -> [String]
splitString s = split s []
   where split s acc
           | null s = acc
           | otherwise = split (drop 16 s) (take 16 s : acc)

-- Combines list contents into unique pairs --
-- ["A","B","C","D"] -> [("A","B") ("A","C") ("A","D") ("B","C") ("B","D") ("C","D")]
combine :: [a] -> [(a, a)]
combine xs = combine' xs []
  where
    pair y = map (\s -> (y, s))
    combine' [] acc = acc
    combine' (z:zs) acc = combine' zs (acc ++ pair z zs)

main :: IO ()
main = do
    hexLines <- lines <$> readFile "8.txt"
    let numEquals = map numEqual hexLines
    let highestEquals = DL.maximumBy (DO.comparing snd) $ zip [1..] numEquals
    putStrLn $ "Line # " ++ show (fst highestEquals) ++ " with a score of " ++ show (snd highestEquals)
