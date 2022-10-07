# Plutus Hello World

This is a bare bones Plutus smart-contract template. The goal is to provide the minimum expression of a Plutus project to be used as starting point to build more complex contracts.

## Dev Environment

To build the script you'll need the Haskell toolchain (GCH, Cabal, etc) and several dependencies from IOHK repositories. Once you've compiled the source code into a Plutus script, you'll need a fully-synced Cardano Node and the `cardano-cli` binary in order to submit example transactions to the Blockchain.

If you don't want to install the required components yourself, you can use [Demeter.run](https://demeter.run) platform to create a cloud environment with access to common Cardano infrastrcuture. The following command will open this repo in a private, web-based VSCode IDE with all of the required Haskell toolchain, access to a shared Cardano Node and a pre-installed binary of the `cardano-cli`.

[![Code in Cardano Workspace](https://demeter.run/code/badge.svg)](https://demeter.run/code?repository=https://github.com/txpipe/plutus-starter-kit.git&template=plutus)

## Quick Start

> **Note**
> This guide assumes that you're using a Cardano Workspace as detailed above. 

The source code for the Plutus contract lives in the `src/Hello` folder. The `Contracts.hs` contains a minimalistic validator logic and the required boilerplate code to serialize the Plutus-Tx code as UPLC that can be submitted on-chain.

The entry point for the Cabal project lives in `Main.hs` and can be used to trigger the serialization. Run the following command from the workspace terminal:

```sh
cabal run plutus-starter-kit -- assets/contract.plutus
```

> **Note**
> The _Cardano Workspace_ provides a cached version of the Cardano api dependencies. This greatly accelerates the build process.

When the command finishes, you should get a `assets/contract.plutus` file that contains a JSON envelope of the UPLC code. This file can be used to submit transactions on-chain.

```json
{
    "type": "PlutusScriptV1",
    "description": "",
    "cborHex": "5907c05907bd0100003232323232323232323...
}
```

To construct on-chain transactions, we'll need the address of the script we've just compiled. For this, run the following command:

```sh
cardano-cli address build \
  --payment-script-file ./assets/contract.plutus \
  --testnet-magic 2 \
  --out-file ./assets/mainnet.addr
```

> **Note**
> The flag `--testnet-magic 2` in the above command assumes that you're working with the `preview` network. If you're working with a different testnet, change the magic value to match your requirements or replace the flag for `--mainnet` if you're connected to mainnet.

## System Requirements

* git@github.com:bitcoin-core/secp256k1.git
