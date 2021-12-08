{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Backend.Cookie where

import Control.Applicative
import Control.Monad.IO.Class (MonadIO (liftIO))
import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (BlockCipher (..), Cipher (..), IV, makeIV)
import Crypto.Error (CryptoFailable (..))
import Crypto.KDF.PBKDF2 (Parameters (..), fastPBKDF2_SHA1)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Debug.Trace (traceM)
import System.Directory (getHomeDirectory)
import qualified System.Keyring as Keyring (Password (Password), Service (Service), Username (Username), getPassword)

data CookieField = CookieField String B.ByteString deriving (Show)

instance FromRow CookieField where
  fromRow = CookieField <$> field <*> field

chromeMacPath :: IO FilePath
chromeMacPath = do
  homePath <- getHomeDirectory
  return $ homePath ++ "/Library/ApplicationSupport/Google/Chrome/Default/Cookies"

getChromeCookie :: IO ()
getChromeCookie = do
  let sql = "select name, encrypted_value from cookies where host_key like \"%leetcode.com\""
  path <- chromeMacPath
  conn <- open path
  result <- query_ conn sql :: IO [(String, B.ByteString)]
  close conn
  let keys = ["LEETCODE_SESSION", "csrftoken"]
  let filterResult = filter (\tup -> fst tup `elem` keys) result
  decrypted <- zip (map fst filterResult) <$> mapM (decryptChromeCookie . snd) filterResult
  return ()

decryptChromeCookie :: B.ByteString -> IO String
decryptChromeCookie byteStr = do
  password <- Keyring.getPassword (Keyring.Service "Chrome Safe Storage") (Keyring.Username "Chrome")
  case password of
    Nothing -> error "Decrypt Cookie Failed"
    Just (Keyring.Password password) -> do
      let salt = "saltysalt" :: String
      let iterations = 1003
      let key = fastPBKDF2_SHA1 (Parameters iterations 16) (BU.fromString password) (BU.fromString salt) :: B.ByteString
      let encryptedValue = B.drop 3 byteStr
      let iv = BU.fromString $ concat $ replicate 16 " "
      let decrypted = decrypt' CBCdecrypt key (makeIV iv) encryptedValue
      let numPad = B.last decrypted
      let decryptedClean = B.take (B.length decrypted - fromIntegral numPad) decrypted
      return $ BU.toString decryptedClean

newtype Key a = Key B.ByteString
  deriving (Show, Eq)

data CipherMode = CBCencrypt | CBCdecrypt | CTR

encrypt' :: CipherMode -> B.ByteString -> Maybe (IV AES128) -> B.ByteString -> B.ByteString
encrypt' mode key iv = cipherEncrypt ctx iv'
  where
    cipherEncrypt = case mode of
      CBCencrypt -> cbcEncrypt
      CBCdecrypt -> cbcDecrypt
      CTR -> ctrCombine
    ctx = cipherInitNoErr blockCipher
    iv' = maybe (error $ "IV length must be " <> show 16 <> " bytes") id iv
    blockCipher = cipherMakeKey (undefined :: AES128) key
    cipherInitNoErr :: BlockCipher c => Key c -> c
    cipherInitNoErr (Key k) = case cipherInit k of
      CryptoPassed a -> a
      CryptoFailed e -> error (show e)
    cipherMakeKey :: Cipher cipher => cipher -> B.ByteString -> Key cipher
    cipherMakeKey _ = Key

decrypt' :: CipherMode -> B.ByteString -> Maybe (IV AES128) -> B.ByteString -> B.ByteString
decrypt' = encrypt'
