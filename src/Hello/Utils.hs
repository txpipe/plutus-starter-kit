{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- {-# LANGUAGE NoImplicitPrelude #-}

module Hello.Utils (writePlutusFile) where

import qualified Cardano.Api as Api
-- Contracts
import qualified Hello.Contract as Contract
--
import PlutusTx.Prelude (Either (..), Maybe (Nothing), ($), (++), (>>=))
import System.FilePath
import Prelude (IO, print, putStrLn)

writePlutusFile :: FilePath -> IO ()
writePlutusFile filePath =
  Api.writeFileTextEnvelope filePath Nothing Contract.serialized >>= \case
    Left err -> print $ Api.displayError err
    Right () -> putStrLn $ "wrote validator to file " ++ filePath