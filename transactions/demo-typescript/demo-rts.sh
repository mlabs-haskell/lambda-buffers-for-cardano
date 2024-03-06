#!/bin/sh

# DESCRIPTION
#
# This shell script does the following.
#   1. Starts `local-cluster` (plutip) where logs are dumped to to stderr and
#      `./local-cluster.log`.
#      Note that we keep plutip's `./local-cluster-info.json`
#   2. Starts `ogmios` on host 127.0.0.1 with port 1337 where logs are dumped
#      to stderr and `ogmios.log`
#   3. Dumps a config file (compatible with the the demo Typescript
#      application) to `./demo-rtsconfig.json` and stdout.
#
#  Note that the config file is dumped to stdout, and is the only thing that is
#  dumped to stdout, so blocking reads on stdout can be used to detect when
#  everything is ready)
#
# ENVIRONMENT
#   - `ADA`: the total number of ADA per UTxO to start the cluter with
#   (default: 69420). See [1].
#   - `UTXOS`: the total number of UTxOs with amount `$ADA`. (default: 1). See
#     [1]
#   - `NUM_WALLETS`: the total number wallets (default: 1). See [1].
#
# REFERENCES
# [1]: Plutip's `local-cluster` CLI reference:
#      https://github.com/mlabs-haskell/plutip/blob/master/local-cluster/README.md

export ADA="${ADA:-69420}"
export UTXOS="${UTXOS:-1}"
export NUM_WALLETS="${NUM_WALLETS:-1}"

# Ensure we kill all the background processes (plutip + ogmios) when we exit
trap 'trap - EXIT && kill -- -$$ && exit 0' EXIT

###########################
# Setting up the plutip cluster
###########################
export WALLET_DIR
WALLET_DIR="$(mktemp -d)"
export NODE_WORKING_DIR
NODE_WORKING_DIR="$(mktemp -d)"

# The directory for plutip's local cluster information see [1]
export LOCAL_CLUSTER_INFO_JSON="./local-cluster-info.json"

# Start the local cluster in the background
1>&2 echo "demo-rts: starting plutip (local-cluster) logging to ./local-cluster.log"
local-cluster \
    --num-wallets "$NUM_WALLETS" \
    --wallet-dir "$WALLET_DIR" \
    --working-dir "$NODE_WORKING_DIR" \
    --dump-info-json "$LOCAL_CLUSTER_INFO_JSON" \
    --ada "$ADA" \
    --utxos "$UTXOS" \
    2>&1 | tee local-cluster.log 1>&2 \
    &

# NOTE(jaredponn): by observation from plutip, we observe that the `pool-1`
# directory exists with the data we wait for in the following lines.

# Busy loop wait until the socket exists and is ready
export NODE_SOCKET="$NODE_WORKING_DIR/pool-1/node.socket"

until test -S "$NODE_SOCKET"
do
    1>&2 echo "demo-rts: waiting for \`local-cluster\` to be ready"
    sleep 1
done

# Busy loop wait until the private keys exist
while test -z "$(find "$WALLET_DIR" -iname "*.skey" | head -n 1)"
do
    1>&2 echo "demo-rts: waiting for private keys to be ready"
    sleep 1
done

# NOTE(jaredponn): just pick a secret key and return that
export SIGNING_KEY_CBOR_HEX
SIGNING_KEY_CBOR_HEX=$(
    SKEY_FILE="$(find "$WALLET_DIR" -iname "*.skey" | head -n 1)"
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
# Creating a runtime configuration file for the TS application
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
wait
