{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Hello.Contract (validator, wrapped, serialized, hash, HelloDatum (..), HelloRedeemer (..)) where
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised))
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import Hello.Shared (validatorHash, wrap)
import qualified Plutus.V2.Ledger.Api as PlutusV2
import PlutusTx
import PlutusTx.Prelude
import Cardano.Api

newtype HelloDatum = HelloDatum Integer
PlutusTx.unstableMakeIsData ''HelloDatum

newtype HelloRedeemer = HelloRedeemer Integer
PlutusTx.unstableMakeIsData ''HelloRedeemer

run :: HelloDatum -> HelloRedeemer -> PlutusV2.ScriptContext -> Bool
run (HelloDatum datum) (HelloRedeemer redeemer) _ = redeemer < datum

-- Entry

wrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapped = wrap run

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [||wrapped||])

serialized :: PlutusScript PlutusScriptV2
serialized = PlutusScriptSerialised . BSS.toShort . BSL.toStrict . serialise $ validator

hash :: PlutusV2.ValidatorHash
hash = validatorHash validator
