module Ottar.Transform (transformOttar, ottarOuts) where

import Text.PrettyPrint.Leijen as PP

import Ottar.Model
import Ottar.Transform.LaTeX
import Ottar.Transform.Ottar

ottarOuts = [fmtLaTeX, fmtOttar]

-- | Try and transform the pattern language into the specified format.
-- If a supported format return a tuple containing the desired file
-- extension, and the transformed specification.
transformOttar :: String
               -> SecComms
               -> Either String (String, Doc)
transformOttar fmt prtcl
    | fmt == fmtLaTeX = Right (extLaTeX, ottar2LaTeX prtcl)
    | fmt == fmtOttar = Right (extOttar, ottar2Ottar prtcl)
    | otherwise     = Left $ "Unsupported Format: " ++ fmt

-- --------------------------------------------------------------------- [ EOF ]
