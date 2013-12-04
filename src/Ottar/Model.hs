module Ottar.Model where

type SecComms = [SecComm]

data SecComm = SecComm {
      num  :: Int,
      from :: String,
      to   :: String,
      ms   :: Messages
    } deriving (Show, Read)

type Messages = [Message]

data Message = Message {
      payload :: String
    } | Identity {
      owner :: String
    } | Nonce {
      owner :: String
    } | Key {
      kvis  :: KeyVisibility,
      ktype :: KeyType,
      owner :: String
    } | Op {
      func :: CryptoOps,
      key  :: Message,
      msgs :: Messages
    } deriving (Show, Read)


data CryptoOps = Encrypt
               | Decrypt
               | Sign
               | Verify
               | Symm
                 deriving (Show, Read, Ord, Enum, Eq)

mkEncrypt :: Message -> Messages -> Message
mkEncrypt = Op Encrypt

mkDecrypt :: Message -> Messages -> Message
mkDecrypt = Op Decrypt

mkSign :: Message -> Messages -> Message
mkSign = Op Encrypt

mkVerify :: Message -> Messages -> Message
mkVerify = Op Decrypt

mkSymm :: Message -> Messages -> Message
mkSymm = Op Symm


data KeyVisibility = Private 
                   | Public
                     deriving (Read, Ord, Enum, Eq)

instance Show KeyVisibility where
    show Private = "Priv"
    show Public  = "Pub"

data KeyType = EncKey
             | DecKey
             | SignKey
             | VerifyKey
             | SymmKey
               deriving (Read, Ord, Enum, Eq)

instance Show KeyType where
   show EncKey    = "Enc"
   show DecKey    = "Dec"
   show SignKey   = "Sig"
   show VerifyKey = "Ver"
   show SymmKey   = "K" 

mkEncKey :: String -> Message 
mkEncKey = Key Public EncKey

mkDecKey :: String -> Message 
mkDecKey = Key Private DecKey

mkSignKey :: String -> Message 
mkSignKey = Key Private SignKey

mkVerifyKey :: String -> Message
mkVerifyKey = Key Public VerifyKey

mkSymmKey :: String -> Message
mkSymmKey = Key Private SymmKey
