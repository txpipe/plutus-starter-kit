here=$(dirname $0)

cardano-cli address key-gen \
    --verification-key-file ${here}/../assets/wallet1.vkey \
    --signing-key-file ${here}/../assets/wallet1.skey

cardano-cli address build \
    --payment-verification-key-file ${here}/../assets/wallet1.vkey \
    --out-file ${here}/../assets/wallet1.addr \
    --testnet-magic $CARDANO_NODE_MAGIC