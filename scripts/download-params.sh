here=$(dirname $0)

cardano-cli query protocol-parameters --testnet-magic $CARDANO_NODE_MAGIC > ${here}/../assets/params.json
