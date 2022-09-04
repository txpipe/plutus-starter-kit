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

module Hello.Contract (validator, wrapped, serialized, hash) where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import Hello.Shared (validatorHash, wrap)
import qualified Plutus.V1.Ledger.Scripts as Scripts
import Plutus.V2.Ledger.Api (ScriptContext)
import PlutusTx
import PlutusTx.Prelude

newtype HelloDatum = HelloDatum
  { test :: Bool
  }

data HelloRedeemer = Yes | No

PlutusTx.unstableMakeIsData ''HelloDatum
PlutusTx.unstableMakeIsData ''HelloRedeemer

run :: HelloDatum -> HelloRedeemer -> ScriptContext -> Bool
run datum redeemer ctx = True

-- Entry

wrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapped = wrap run

validator :: Scripts.Validator
validator = Scripts.mkValidatorScript $$(PlutusTx.compile [||wrapped||])

serialized :: PlutusScript PlutusScriptV1
serialized = PlutusScriptSerialised . BSS.toShort . BSL.toStrict . serialise $ validator

hash :: Scripts.ValidatorHash
hash = validatorHash validator
