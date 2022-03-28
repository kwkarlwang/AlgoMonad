{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wunused-imports #-}

module Backend.Cookie where

import Control.Lens ((^?))
import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (BlockCipher (..), Cipher (..), IV, makeIV)
import Crypto.Error (CryptoFailable (..))
import Crypto.KDF.PBKDF2 (Parameters (..), fastPBKDF2_SHA1)
import Data.Aeson (Value (String), object, (.=))
import Data.Aeson.Lens (key)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Database.SQLite.Simple
import System.Directory (doesFileExist, getHomeDirectory)
import qualified System.Process as P (readProcess)

getConfigPath :: IO FilePath
getConfigPath = do
  homePath <- getHomeDirectory
  return $ homePath ++ "/.algomonad.yaml"

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
  case length filterResult of
    2 -> do
      decrypted <- zip (map fst filterResult) <$> mapM (decryptChromeCookie . snd) filterResult
      writeCookieToFile decrypted
    _ -> error "Login to leetcode from Chrome and try again"

decryptChromeCookie :: B.ByteString -> IO String
decryptChromeCookie byteStr = do
  password <- init <$> P.readProcess "security" ["find-generic-password", "-w", "-s", "Chrome Safe Storage", "-a", "Chrome"] []
  let salt = "saltysalt" :: String
  let iterations = 1003
  let key = fastPBKDF2_SHA1 (Parameters iterations 16) (BU.fromString password) (BU.fromString salt) :: B.ByteString
  let encryptedValue = B.drop 3 byteStr
  let iv = BU.fromString $ concat $ replicate 16 " "
  let decrypted = decrypt' CBCdecrypt key (makeIV iv) encryptedValue
  let numPad = B.last decrypted
  let decryptedClean = B.take (B.length decrypted - fromIntegral numPad) decrypted
  return $ BU.toString decryptedClean

writeCookieToFile :: [(String, String)] -> IO ()
writeCookieToFile pairs = do
  let jsonObject = object (map (\tup -> T.pack (fst tup) .= T.pack (snd tup)) pairs)
  configPath <- getConfigPath
  Yaml.encodeFile configPath jsonObject

getConfigFromFile :: IO (B.ByteString, B.ByteString)
getConfigFromFile = do
  configPath <- getConfigPath
  bool <- doesFileExist configPath
  if bool
    then
      ( do
          eitherDecode <- Yaml.decodeFile configPath :: IO (Maybe Value)
          case eitherDecode of
            Nothing -> do
              error "Cannot acquire credientials"
            Just yamlValue -> do
              let csrfToken = yamlValue ^? key "csrftoken"
              let leetcodeSession = yamlValue ^? key "LEETCODE_SESSION"
              case (csrfToken, leetcodeSession) of
                (Just (String csrfToken), Just (String leetcodeSession)) -> do
                  return (BU.fromString $ T.unpack csrfToken, BU.fromString $ T.unpack leetcodeSession)
                (_, _) -> error "Cannot acquire credientials"
      )
    else error "Please create config file"

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
