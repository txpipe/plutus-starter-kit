{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Hello.Utils (writePlutusFile) where

import qualified Ledger.Typed.Scripts as Scripts

-------------------------------------------------------------------------------
-- Entry Points
-------------------------------------------------------------------------------

swapWrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
swapWrapped = wrap swapValidator

validator :: Scripts.Validator
validator = mkValidatorScript $$(PlutusTx.compile [||swapWrapped||])

swap :: PlutusScript PlutusScriptV1
swap = PlutusScriptSerialised . SBS.toShort . LB.toStrict . serialise $ validator

swapHash :: ValidatorHash
swapHash = validatorHash validator

writePlutusFile :: FilePath -> IO ()
writePlutusFile filePath =
  Api.writeFileTextEnvelope filePath Nothing swap >>= \case
    Left err -> print $ Api.displayError err
    Right () -> putStrLn $ "wrote NFT validator to file " ++ filePath