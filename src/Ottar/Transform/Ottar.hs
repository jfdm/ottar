module Ottar.Transform.Ottar (ottar2Ottar, fmtOttar, extOttar) where

import Text.PrettyPrint.Leijen as PP
import Ottar.Model

fmtOttar = "ottar"
extOttar = ".ottar"

ottar2Ottar :: SecComms -> Doc
ottar2Ottar prtcl = vsep $ map prettyStep prtcl

prettyStep :: SecComm -> Doc
prettyStep (SecComm i f t ms) = text f <+>
                                text "->" <+>
                                text t <+>
                                colon <+>
                                prettyAsciiMSGs ms

prettyAsciiMSGs :: Messages -> Doc
prettyAsciiMSGs ms = hsep $ punctuate comma $ map prettyAsciiMSG ms

prettyAsciiMSG :: Message -> Doc
prettyAsciiMSG (Identity i) = prettyAtom "ID" i
prettyAsciiMSG (Nonce n)    = prettyAtom "N" n
prettyAsciiMSG (Key v t o)  = prettyAtom (show t) o
prettyAsciiMSG (Op _ k ms)  = prettyOps k ms
prettyAsciiMSG (Message c)  = dquotes $ text c

-- ------------------------------------------------------- [ Pretty Operations ]

prettyOps :: Message -> Messages -> Doc
prettyOps (Key v t o) ms = braces (prettyAsciiMSGs ms) <>
                           text "_" <>
                           prettyAtom (show t) o
-- -------------------------------------------------------------------- [ Misc ]
prettyAtom :: String -> String -> Doc
prettyAtom k v = text k <> parens (text v)
