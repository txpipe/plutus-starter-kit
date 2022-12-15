# Plutus Hello World

This is a bare bones Plutus smart-contract template. The goal is to provide the minimum expression of a Plutus project to be used as starting point to build more complex contracts.

## Dev Environment

To build the script you'll need the Haskell toolchain (GCH, Cabal, etc) and several dependencies from IOHK repositories. There's also a requirement on the [secp256k1](https://github.com/bitcoin-core/secp256k1.git) library. Once you've compiled the source code into a Plutus script, you'll need a fully-synced Cardano Node and the `cardano-cli` binary in order to submit example transactions to the Blockchain.

If you don't want to install the required components yourself, you can use [Demeter.run](https://demeter.run) platform to create a cloud environment with access to common Cardano infrastrcuture. The following command will open this repo in a private, web-based VSCode IDE with all of the required Haskell toolchain, access to a shared Cardano Node and a pre-installed binary of the `cardano-cli`.

[![Code in Cardano Workspace](https://demeter.run/code/badge.svg)](https://demeter.run/code?repository=https://github.com/txpipe/plutus-starter-kit.git&template=plutus)

## Quick Start

> **Note**
> This guide assumes that you're using a Cardano Workspace as detailed above. 

### Compile the Validator

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
    "type": "PlutusScriptV2",
    "description": "",
    "cborHex": "5907c35907c001000..."
}
```

To construct on-chain transactions, we'll need the address of the script we've just compiled. For this, run the following command:

```sh
cardano-cli address build \
  --payment-script-file ./assets/contract.plutus \
  --testnet-magic ${CARDANO_NODE_MAGIC} \
  --out-file ./assets/contract.addr
```

> **Note**
> The `CARDANO_NODE_MAGIC` env variable is set automatically by the Cardano Workspace.

If you want to query the balance of your script address, we added a helper script that queries the UTxO for the address generated in the previous step. Open your Cardano Workspace terminal and run the following command:

```sh
./scripts/contract-balance.sh
```

> **Note**
> Querying the node for UTxO is not part of the scope for this Starter Kit. If you want to learn more about common operations through the CLI, try out the [Cardano-CLI Starter Kit](https://github.com/txpipe/cardano-cli-starter-kit)

### Prepare a Dev Wallet

To interact with our contract on-chain, we'll need a wallet with some tAda (test ADA). Open your Cardano Workspace terminal and excute the following command:

```sh
./scripts/new-dev-wallet.sh
```

> **Note**
> Creating wallet keys is not part of the scope for this Starter Kit. If you want to learn more about common operations through the CLI, try out the [Cardano-CLI Starter Kit](https://github.com/txpipe/cardano-cli-starter-kit)

When done, you should see new files inside the `assets` directory that contain the key pairs and address for your newly created wallet.

If you want to query the balance of your wallet address, you can use a helper script that queries the UTxO for the address generated in the previous step. Open your Cardano Workspace terminal and run the following command:

```sh
./scripts/dev-wallet-balance.sh
```

> **Note**
> Querying the node for UTxO is not part of the scope for this Starter Kit. If you want to learn more about common operations through the CLI, try out the [Cardano-CLI Starter Kit](https://github.com/txpipe/cardano-cli-starter-kit)

Your wallet we'll need some funds to interact with the contract. You can use the corresponding [faucet](https://docs.cardano.org/cardano-testnet/tools/faucet) to obtain some.

### Lock Funds

Now that we have a validator script ready, we'll lock funds on the script address. Locking funds is just a fancy way of saying that we'll send some assests (ADA in this case) to the script by submitting a transaction to the corresponding address. It is called "locking" because the funds can only be retrieved if the validator script allows it.

Our very basic validator has one simple task: to ensure that the integer value in the datum is greater or equal than the integer value in the redeemer (quite dumb and useless).

Our datum is defined as a Haskell newtype that wraps a single integer. The `src/Hello/Contract.hs` contains the correponding code:

```haskell
newtype HelloDatum = HelloDatum Integer
```

When locking the funds, the submitting party is the one in control of the datum and needs to specify the hash of the value. For that, we need a JSON representation of the datum to be passed to the cardano-cli in order to obtain the hash. The file `assets/lock.datum` contains an example of the JSON representation for a datum that holds the value `42`:

```json
{"constructor":0,"fields":[{"int":42}]}
```

From inside your Cardano Workspace, open a terminal and execute the following command to generate a hash for data contained in the `assets/lock.datum` file. The result of the cardano-cli command will be stored in the `scriptdatumhash` variable.

```sh
scriptdatumhash=$(cardano-cli transaction hash-script-data --script-data-file assets/lock.datum)
```

The locking transaction needs a reference to a UTxO in your dev wallet to be used as source for the funds we'll be locking in the script. Since this step is specific to the state of your wallet, you'll need to manually assign the value in a shell variable for the next step to succeed.

Use the `dev-wallet-balance.sh` to check your available UTxO and select the one to be used in our test transaction. Assign the `{TxHash}#{TxIn}` to the `locktxin` shell variable. We'll be locking a small amount of tADA (1230000 lovelace), make sure that your UTxO has enough. The following is just an example, replace accordingly:

```sh
$ ./scripts/dev-wallet-balance.sh

                           TxHash                                 TxIx   Amount
---------------------------------------------------------------------------------
0939be18d8583bbdd7309b4cfefd419c8900df0f84142149066ec2755c94a322     0   9980637126 lovelace
9805cc2d7c08f8b99acd2d60d9cf1e3eb14b281e7f3f430f26a26f0927ff5fde     0   1060942 lovelace
9ec2a9a546d8a9c7221be452e26278d2128cb39429d57a58b420598c0e9c2591     0   1060678 lovelace

$ locktxin=0939be18d8583bbdd7309b4cfefd419c8900df0f84142149066ec2755c94a322#0
```

We also need to retrieve some Protocol Parameters before building the transaction, in order to do so, execute the following script helper:

```sh
$ ./scripts/download-params.sh
```

Now we are finally ready to build the locking transaction. From inside your Cardano Workspace, open a terminal and execute the following command to build the unsigned Tx payload. 

```sh
cardano-cli transaction build \
  --babbage-era \
  --testnet-magic ${CARDANO_NODE_MAGIC} \
  --change-address $(cat assets/wallet1.addr) \
  --tx-in ${locktxin} \
  --tx-out $(cat assets/contract.addr)+1230000 \
  --tx-out-datum-hash ${scriptdatumhash} \
  --protocol-params-file assets/params.json \
  --out-file assets/lock.tx
```

> **Note**
> The `CARDANO_NODE_MAGIC` env variable is set automatically by the Cardano Workspace.

The next step consists of signing the transaction with our dev wallet key. From inside your Cardano Workspace, open a terminal and execute the following command:

```sh
cardano-cli transaction sign \
  --tx-body-file assets/lock.tx \
  --signing-key-file assets/wallet1.skey \
  --testnet-magic ${CARDANO_NODE_MAGIC} \
  --out-file assets/lock.tx-signed
```

The only remaining task is to submit the signed transaction on-chain. From inside your Cardano Workspace, open a terminal and execute the following command:

```sh
cardano-cli transaction submit \
  --testnet-magic ${CARDANO_NODE_MAGIC} \
  --tx-file assets/lock.tx-signed
```

After a few seconds (might be longer depending on chain activity), the balance for the script address should show our locked funds. Check the UTxO of the script address using our helper script:

```sh
./scripts/contract-balance.sh
```

The output of the script should show something similar to the following:

```sh
 TxHash    TxIx        Amount
--------------------------------------------------------------------------------------
b00...313     1        1230000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "923...4ec"
```

# Unlock Funds

To unlock the assets, we need to prepare a transaction that consumes the UTxO from the contract address and including a redeemer that complies with the contraints defined by our validator.

Query the contract balance once more, take the TxHash and TxIx and store it inside the variable `lockedtxin`.

```sh
$ ./scripts/contract-balance.sh
 TxHash    TxIx        Amount
--------------------------------------------------------------------------------------
8af...ee4     0        1230000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "923...4ec"

$ lockedtxin=8afb82a260fc3c0fd4e5828da171b4ae52144669c2ec915df846cff6a628dee4#0
```

We also need to specify some collateral UTxO that the protocol can use in case our validator fails. For this, query the balance of your wallet again, select an available UTxO and store it in the variable `collateraltxin`. 

```sh
$ ./scripts/dev-wallet-balance.sh

                           TxHash                                 TxIx   Amount
---------------------------------------------------------------------------------
0939be18d8583bbdd7309b4cfefd419c8900df0f84142149066ec2755c94a322     0   9980637126 lovelace
9805cc2d7c08f8b99acd2d60d9cf1e3eb14b281e7f3f430f26a26f0927ff5fde     0   1060942 lovelace
9ec2a9a546d8a9c7221be452e26278d2128cb39429d57a58b420598c0e9c2591     0   1060678 lovelace

$ collateraltxin=0939be18d8583bbdd7309b4cfefd419c8900df0f84142149066ec2755c94a322#0
```

```sh
cardano-cli transaction build \
  --babbage-era \
  --testnet-magic ${CARDANO_NODE_MAGIC} \
  --tx-in ${lockedtxin} \
  --tx-in-script-file assets/contract.plutus \
  --tx-in-datum-file assets/lock.datum \
  --tx-in-redeemer-file assets/unlock.redeemer \
  --change-address $(cat assets/wallet1.addr) \
  --tx-in-collateral ${collateraltxin} \
  --protocol-params-file assets/params.json \
  --out-file assets/unlock.tx
```

As usual, the next step consists of signing the transaction with our dev wallet key. From inside your Cardano Workspace, open a terminal and execute the following command:

```sh
cardano-cli transaction sign \
  --tx-body-file assets/unlock.tx \
  --signing-key-file assets/wallet1.skey \
  --testnet-magic ${CARDANO_NODE_MAGIC} \
  --out-file assets/unlock.tx-signed
```

Now we need to submit the signed transaction on-chain. From inside your Cardano Workspace, open a terminal and execute the following command:

```sh
cardano-cli transaction submit --testnet-magic ${CARDANO_NODE_MAGIC} --tx-file assets/unlock.tx-signed
```

After a few seconds, assuming that the submission was successful, you should see the unlocked assets back in your dev wallet. Use the following command to check your wallet balance. You should see the new UTxO.

```sh
./scripts/dev-wallet-balance.sh
```