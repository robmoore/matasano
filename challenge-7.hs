-- The Base64-encoded content in this file has been encrypted via AES-128 in ECB mode under the key
-- "YELLOW SUBMARINE". Decrypt it. You know the key, after all.
-- http://cryptopals.com/sets/1/challenges/7/

import qualified Crypto.Cipher.Types as CT
import qualified Crypto.Cipher.AES as CA
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as BB (decode)

keyString :: B.ByteString
keyString = BC.pack "YELLOW SUBMARINE"

initAES128 :: B.ByteString -> CA.AES
initAES128 = either (error . show) CT.cipherInit . CT.makeKey

decrypt :: CA.AES -> BC.ByteString -> BC.ByteString
decrypt ctx msg =
	either (error . show) (CA.decryptECB ctx) decodedMsg
	where decodedMsg = BB.decode msg

main = do
	    file <- BC.readFile "7.txt"
	    let pText = decrypt ctx $ BC.concat $ BC.lines file
	    putStrLn $ BC.unpack pText
  where ctx = initAES128 keyString
      