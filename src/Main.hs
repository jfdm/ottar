-- | Main Program reads the command line options, checks the supplied
-- sif file, and pushes the dot version out to STDOUT.
{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where

import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.IO
import System.FilePath
import System.Exit

import Text.Show.Pretty
import Text.PrettyPrint.Leijen as PP

import Ottar.Parser
import Ottar.Transform

-- | Get AST andn spit out a LaTeX representation.
main :: IO ()
main = do
  args <- getArgs
  opts <- (if null args
           then withArgs ["--help"]
           else id) $ cmdArgs ottarOptions

  contents <- readFile $ head args
  case parseOttarInput contents of
    Left err  -> do
         putStrLn $ file opts ++ " failed to parse"
         print err
         exitWith (ExitFailure 1)
    Right res -> do
         if not (model opts)
         then putStrLn ""
         else do
           -- if requested print the model
           let fname = addExtension (file opts) ".model"
           writeFile fname (ppShow res)
           putStrLn $ "Model for: "
                                ++ file opts
                                ++ " has been written in "
                                ++ fname
           -- if requested save the output to file.
         let outfmt = case null (to opts) of
                        True  -> head ottarOuts
                        False -> (to opts) 
         case transformOttar outfmt res of
           Left err -> do
                   print err
                   exitWith (ExitFailure 1)
           Right (ext, doc) -> do
                   if null (output opts)
                      then print $ doc
                      else do
                        let fname = addExtension (output opts) ext
                        handle <- openFile fname WriteMode
                        hPutDoc handle doc
                        hClose handle
                        putStrLn $ "File: "
                                ++ fname
                                ++ " has been written."
                   exitSuccess

-- ----------------------------------------------------------------- [ Options ]

-- | Options the type checker takes.
data OttarOptions = OttarOptions {
      to     :: String,   -- ^ The output format
      output :: FilePath, -- ^ Output filename.
      model  :: Bool,     -- ^ Print the resulting model
      file   :: FilePath  -- ^ The input file.
    } deriving (Show, Data, Typeable)

-- | Set default options
ottarOptions :: OttarOptions
ottarOptions = OttarOptions {
               to = def
                 &= typ "FORMAT"
                 &= help "Format to output",
               output = def
                     &= typ "FILENAME"
                     &= help "The name of file to write the output too.",
               model = def
                    &= help "Save the Model",
               file = def
                   &= typFile
                   &= argPos 0
             }
             &= summary "Ottar (C) Jan de Muijnck-Hughes 2013"
             &= program "ottar"
             &= details ["Ottar is a tool to parse informal security protocol narrations into other formats."
                        ]
-- --------------------------------------------------------------------- [ EOF ]
