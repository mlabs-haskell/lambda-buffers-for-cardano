#!/bin/sh

# This shell script does the following.
#   - 

# References:
# [1]: Plutip's `local-cluster` CLI reference:
# https://github.com/mlabs-haskell/plutip/blob/master/local-cluster/README.md

# Do nothing if we already have a plutip cluster initialized
# if test -n "$NODE_SOCKET"
# then
#     2>&1 echo "Plutip is already initialized at $NODE_SOCKET, so doing nothing"
#     exit 0
# fi


###########################
# Setting up the plutip cluster
###########################
# Set the variables to temporary directories (if not set already)
# export WALLET_DIR="${WALLET_DIR:-$(mktemp -d)}"
# export NODE_WORKING_DIR="${NODE_WORKING_DIR:-$(mktemp -d)}"
export WALLET_DIR
WALLET_DIR="$(mktemp -d)"
export NODE_WORKING_DIR
NODE_WORKING_DIR="$(mktemp -d)"

# The directory for plutip's local cluster information see [1]
export LOCAL_CLUSTER_INFO_JSON="./local-cluster-info.json"

# Ensure $LOCAL_CLUSTER_INFO_JSON exists because plutip doesn't like it if this
# file doesn't already exist.
# Note we initial remove $LOCAL_CLUSTER_INFO_JSON so the busy loop (to wait for
# the node is initialized) will wait if the file exists already.
rm -f "$LOCAL_CLUSTER_INFO_JSON" && touch "$LOCAL_CLUSTER_INFO_JSON"

export NODE_SOCKET="/tmp/"

# Start the local cluster in the background
2>&1 echo "demo-rts: starting plutip (local-cluster) in the background"
local-cluster \
    --num-wallets 1 \
    --wallet-dir "$WALLET_DIR" \
    --working-dir "$NODE_WORKING_DIR" \
    --dump-info-json "$LOCAL_CLUSTER_INFO_JSON" \
    --ada 694200 \
    --utxos 1 \
    1>local-cluster.log 2>&1 \
    &

# Busy loop wait until the socket exists and is ready
export NODE_SOCKET="$NODE_WORKING_DIR/pool-1/node.socket"

until test -S "$NODE_SOCKET"
do
    2>&1 echo "demo-rts: waiting for \`local-cluster\` to be ready..."
    sleep 1
done

while test -z "$(find "$WALLET_DIR" -iname "*.skey")"
do
    2>&1 echo "demo-rts: waiting private keys to be ready..."
    sleep 1
done

export SIGNING_KEY_CBOR_HEX
SIGNING_KEY_CBOR_HEX=$(
    SKEY_FILE="$(find "$WALLET_DIR" -iname "*.skey")"
    jq -r ".cborHex" < "$SKEY_FILE"
    )

###########################
# Setting up ogmios
###########################
export OGMIOS_PORT="1337"
export OGMIOS_HOST="127.0.0.1"

2>&1 echo "demo-rts: starting ogmios in the background with host \`$OGMIOS_HOST\` on port \`$OGMIOS_PORT\`"
ogmios \
    --host "$OGMIOS_HOST" \
    --port "$OGMIOS_PORT" \
    --node-socket "$NODE_SOCKET" \
    --node-config "$(echo "$NODE_SOCKET" | sed -e 's/\.socket$/\.config/g')" \
    1>ogmios.log 2>&1 \
    &
