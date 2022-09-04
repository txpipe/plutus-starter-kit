module Main where

import Hello.Contract

type Opts = FilePath

opts :: ParserInfo Opts
opts =
  let parseOpts = argument str . mconcat $ [metavar "FILE", help "File to which the plutus script will be written"]
   in info (parseOpts <**> helper) . mconcat $ [fullDesc, progDesc "Create a smart contract for bulk purchases"]

main :: IO ()
main = execParser opts >>= writePlutusFile
