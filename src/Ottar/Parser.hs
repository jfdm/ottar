module Ottar.Parser where

import Text.Parsec

import Data.Maybe
import Data.List

import Ottar.Lexer
import Ottar.Model

-- | Wrapper function.
parseOttarInput :: String -> Either ParseError SecComms
parseOttarInput input = runParser (runLex doParse) 1 "" input
                        
doParse :: Parser SecComms
doParse = do steps <- many parseStep
             eof
             return $ steps
          <?> "Parse Protocol"

parseStep :: Parser SecComm
parseStep = do from <- identifier
               reservedOp "->"
               to <- identifier
               colon
               msgs <- commaSep1 parseMSGs
               no <- getState
               updateState (+1)
               return $ SecComm no from to msgs
            <?> "Protocol Step"

parseMSGs :: Parser Message
parseMSGs = do try parseFunctionAtom
        <|> parseMSGAtom
        <?> "Message"

-- --------------------------------------------------------- [ Parse Functions ]
parseFunctionAtom :: Parser Message
parseFunctionAtom = do msgs <- braces $ commaSep1 $ parseMSGs
                       reservedOp "_"
                       key <- parseKey
                       let optype = case ktype key of
                                      EncKey    -> Encrypt
                                      DecKey    -> Decrypt
                                      SignKey   -> Encrypt
                                      VerifyKey -> Decrypt
                                      SymmKey   -> Symm
                       return $ Op optype key msgs
                  <?> "Function"

-- ----------------------------------------------- [ Parse Individual Messages ]
parseMSGAtom :: Parser Message
parseMSGAtom = do try parseNonce
           <|> parseIdentity
           <|> parseRawMsg
           <|> parseKey
           <?> "Msg Atomic"

-- ------------------------------------------- [ Parse Nonce's, ID, & Raw Msgs ]
parseNonce :: Parser Message
parseNonce = do reserved "N"
                orig <- parens identifier
                return $ Nonce orig

parseIdentity :: Parser Message
parseIdentity = do reserved "ID"
                   orig <- parens identifier
                   return $ Identity orig

parseRawMsg :: Parser Message
parseRawMsg = do msg <- stringLiteral
                 return $ Message msg

-- -------------------------------------------------------------------- [ Keys ]
parseKey :: Parser Message
parseKey = do try parseDecKey
          <|> parseEncKey
          <|> parseSigKey
          <|> parseVerKey
          <|> parseSymmKey
          <?> "Keys"


parseSymmKey :: Parser Message
parseSymmKey = do reserved "K"
                  orig <- parens identifier
                  return $ mkSymmKey orig

parseEncKey :: Parser Message
parseEncKey = do reserved "Enc"
                 orig <- parens identifier
                 return $ mkEncKey orig

parseDecKey :: Parser Message
parseDecKey = do reserved "Dec"
                 orig <- parens identifier
                 return $ mkDecKey orig

parseSigKey :: Parser Message
parseSigKey = do reserved "Sig"
                 orig <- parens identifier
                 return $ mkSignKey orig

parseVerKey :: Parser Message
parseVerKey = do reserved "Ver"
                 orig <- parens identifier
                 return $ mkVerifyKey orig



-- --------------------------------------------------------------------- [ EOF ]

