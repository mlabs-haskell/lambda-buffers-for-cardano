#!/bin/sh

# This shell script does the following.
#   1. Starts `local-cluster` (plutip) where logs are dumped to to stderr and
#      `./local-cluster.log`.
#      Note that we keep plutip's `./local-cluster-info.json`
#   2. Starts `ogmios` on host 127.0.0.1 with port 1337 where logs are dumped
#      to stderr and `ogmios.log`
#   3. Dumps a config file (compatible with the the `demo-rts` Typescript
#   application) to `./demo-rtsconfig.json`.
#   It also dumps this config file to stdout (and is the only thing that is
#   dumped to stdout, so blocking reads on stdout can be used to detect when
#   everything is ready)


# References:
# [1]: Plutip's `local-cluster` CLI reference:
#      https://github.com/mlabs-haskell/plutip/blob/master/local-cluster/README.md

###########################
# Setting up the plutip cluster
###########################
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
1>&2 echo "demo-rts: starting plutip (local-cluster) logging to ./local-cluster.log"
local-cluster \
    --num-wallets 1 \
    --wallet-dir "$WALLET_DIR" \
    --working-dir "$NODE_WORKING_DIR" \
    --dump-info-json "$LOCAL_CLUSTER_INFO_JSON" \
    --ada 694200 \
    --utxos 1 \
    2>&1 | tee local-cluster.log 1>&2 \
    &

# Busy loop wait until the socket exists and is ready
export NODE_SOCKET="$NODE_WORKING_DIR/pool-1/node.socket"

until test -S "$NODE_SOCKET"
do
    1>&2 echo "demo-rts: waiting for \`local-cluster\` to be ready"
    sleep 1
done

# Busy loop wait until the private keys exist
while test -z "$(find "$WALLET_DIR" -iname "*.skey")"
do
    1>&2 echo "demo-rts: waiting for private keys to be ready"
    sleep 1
done

# NOTE(jaredponn): we know that there is only one file with the `.skey` suffix
# because we initialized plutip to create only one wallet above for us.
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

1>&2 echo "demo-rts: starting ogmios in the background with host \`$OGMIOS_HOST\` on port \`$OGMIOS_PORT\` logging to ./ogmios.log"
# shellcheck disable=SC2001
ogmios \
    --host "$OGMIOS_HOST" \
    --port "$OGMIOS_PORT" \
    --node-socket "$NODE_SOCKET" \
    --node-config "$(echo "$NODE_SOCKET" | sed -e 's/\.socket$/\.config/')" \
    2>&1 | tee ogmios.log 1>&2 \
    &

###########################
# Creating a configuration file for the TS application
###########################
export DEMO_RTS_CONFIG_JSON="./demo-rtsconfig.json"

1>&2 echo "demo-rts: creating \`$DEMO_RTS_CONFIG_JSON\`"
jq -n '{ ogmios: { host: $ogmiosHost, port: $ogmiosPort }, signingKeyCborHex: $signingKey }' \
    --arg ogmiosHost "$OGMIOS_HOST" \
    --arg ogmiosPort "$OGMIOS_PORT" \
    --arg signingKey "$SIGNING_KEY_CBOR_HEX" \
    | tee "$DEMO_RTS_CONFIG_JSON"

###########################
# Wait for plutip + ogmios to finish (they will never finish)
###########################

# shellcheck disable=SC2046
wait $(jobs -p)
