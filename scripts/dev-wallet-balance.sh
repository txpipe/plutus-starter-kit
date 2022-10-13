here=$(dirname $0)

cardano-cli query utxo \
    --address $(cat ${here}/../assets/wallet1.addr) \
    --testnet-magic $CARDANO_NODE_MAGIC
