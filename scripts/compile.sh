set -eu
thisDir=$(dirname "$0")
mainDir=$thisDir/..

cd $mainDir

cabal run plutus-starter-kit -- assets/contract.plutus

cardano-cli address build \
  --payment-script-file ./assets/contract.plutus \
  --mainnet \
  --out-file ./assets/mainnet.addr

cardano-cli address build \
  --payment-script-file ./assets/contract.plutus \
  --testnet-magic 1097911063 \
  --out-file ./assets/testnet.addr