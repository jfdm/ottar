module Ottar.Transform.LaTeX (ottar2LaTeX, fmtLaTeX, extLaTeX) where

import Text.PrettyPrint.Leijen as PP
import Data.Maybe

import Ottar.Model

fmtLaTeX = "latex"
extLaTeX = ".tex"

ottar2LaTeX :: SecComms -> Doc
ottar2LaTeX prtcl = prettyEnv "enumerate" $ vsep $ map prettyStep prtcl

prettyStep :: SecComm -> Doc
prettyStep s = prettyCMD "item" <>
               prettySend (from s) (to s) msgs
    where
      msgs = prettyLaTeXMSGs (ms s)


prettyLaTeXMSGs :: Messages -> Doc
prettyLaTeXMSGs ms = hsep $ punctuate comma $ map prettyLaTeXMSG ms

prettyLaTeXMSG :: Message -> Doc
prettyLaTeXMSG (Identity i) = prettyAtom "ID" i
prettyLaTeXMSG (Nonce n)    = prettyAtom "N" n
prettyLaTeXMSG (Key v t o)  = prettyKey v t o
prettyLaTeXMSG (Op _ k ms)  = prettyOps k ms
prettyLaTeXMSG (Message c)  = prettyLaTeX "mathrm" (ldquotes (text c))

-- ------------------------------------------------------------- [ Pretty Keys ]
prettyKey :: KeyVisibility -> KeyType -> String ->Doc
prettyKey vis ktype own = case ktype of
                            SymmKey   -> lower (text "K") (text own)
                            otherwise -> prettyKey' ktype vis own
    where
      prettyKey' n vis id = prettyLaTeX "mathsf" (text (show n)) <> parens (text id)

-- ------------------------------------------------------- [ Pretty Operations ]

prettyOps :: Message -> Messages -> Doc
prettyOps (Key v t o) ms = lower body exp
    where
      body = (laTeXbraces $ prettyLaTeXMSGs ms)
      exp = (prettyKey v t o)

-- -------------------------------------------------------------------- [ Misc ]
prettyAtom :: String -> String -> Doc
prettyAtom k v = lower (text k) (text v) 

prettySend :: String -> String -> Doc -> Doc
prettySend a b msg = prettyLaTeX "ensuremath" (hsep [arrow, msg])
    where
      arrow = text a <> prettyCMD "rightarrow" <+> text b <> colon

-- ------------------------------------------------------ [ Pretty LaTeX Stuff ]
prettyEnv :: String -> Doc -> Doc
prettyEnv naam body = vsep [b, indent 2 body, e]
    where
      b = prettyLaTeX "begin" n
      e = prettyLaTeX "end"   n
      n = text naam

prettyLaTeX :: String -> Doc -> Doc
prettyLaTeX env body = prettyCMD env <>
                       braces body

prettyCMD :: String -> Doc
prettyCMD naam = backslash <>
                 text naam

lower :: Doc -> Doc -> Doc
lower = script "_"

raise :: Doc -> Doc -> Doc
raise = script "^"

script :: String -> Doc -> Doc -> Doc
script s b e = b <> text s <> braces e

laTeXbraces :: Doc -> Doc
laTeXbraces = enclose (backslash <> lbrace) (backslash <> rbrace)

lsquotes :: Doc -> Doc
lsquotes = enclose (text "`") squote

ldquotes :: Doc -> Doc
ldquotes b = lsquotes $ lsquotes b
