/**
 * Implementation for ogmios to satisfy the {@link Submit} and {@link Query}
 * interface
 */

import {
  createConnectionObject,
  createInteractionContext,
  createLedgerStateQueryClient,
  createTransactionSubmissionClient,
  getServerHealth,
  InteractionContext,
} from "@cardano-ogmios/client";
import * as ogmios from "@cardano-ogmios/schema";
import * as csl from "@emurgo/cardano-serialization-lib-nodejs";
import { TxBuilderConfig, TxExUnits } from "./tx-builder.js";
import { ogmiosUtxoToCslUtxo } from "./utils.js";
import { Submit } from "./submit.js";
import { Query } from "./query.js";
import * as timers from "node:timers/promises";

/**
 * A wrapper around Ogmios.
 *
 * @privateremarks
 * TODO(jaredponn): it's probably better to just cook up a websocket connection
 * and connect to ogmios directly (avoiding their TS API). There are numerous
 * instances where the TS types disagree with what is actually is provided by
 * ogmios (perhaps this is as versioning mismatch on our end?)
 *
 * Also, it would be better to manage the connection manually to use one
 * websocket connection for everything instead of spinning up a new websocket
 * for every request. It's a lot easier to manage this if we spin up the
 * websocket ourselves :^) and make the caller manually close the socket to
 * finish things up.
 *
 * Moreover, it would be a good idea to acquire a ledger state for the entire
 * ogmios session (again! it's helpful if we manage our own connection and its
 * lifetime!) s.t. we have a consistent view of the UTxOs
 */
export class Ogmios implements Query, Submit {
  #connection: { host: string; port: number };

  /**
   * @internal
   */
  constructor(connection: { host: string; port: number }) {
    this.#connection = connection;
  }

  /**
   * Creates a new connection to ogmios running on the provided host and port
   */
  static async new(connection: {
    host: string;
    port: number;
  }): Promise<Ogmios> {
    const finalConnection = await Promise.resolve(connection);
    return new Ogmios(finalConnection);
  }

  async createInteractionContext(): Promise<InteractionContext> {
    const context = await createInteractionContext(
      (err) => {
        throw err;
      },
      () => {
        return;
      },
      { connection: this.#connection },
    );
    return context;
  }

  async isSynced(delay = 1000): Promise<void> {
    const connectionObject = createConnectionObject(this.#connection);
    let serverHealth = await getServerHealth({ connection: connectionObject });
    let syncedAmount = serverHealth.networkSynchronization;

    while (!(Math.abs(syncedAmount - 1) <= 0.01)) {
      serverHealth = await getServerHealth({ connection: connectionObject });
      syncedAmount = serverHealth.networkSynchronization;
      await timers.setTimeout(delay);
    }

    return;
  }

  async queryAddressUtxos(
    addr: csl.Address,
  ): Promise<csl.TransactionUnspentOutputs> {
    const context = await this.createInteractionContext();
    const client = await createLedgerStateQueryClient(context);
    try {
      const utxos = await client
        .utxo({ addresses: [addr.to_bech32()] })
        .then(ogmiosUtxoToCslUtxo);
      return utxos;
    } finally {
      client.shutdown();
    }
  }

  async queryUtxo(
    transactionInput: csl.TransactionInput,
  ): Promise<csl.TransactionUnspentOutput | undefined> {
    const transactionInputJson = transactionInput.to_js_value();

    const context = await this.createInteractionContext();
    const client = await createLedgerStateQueryClient(context);
    try {
      const utxos = await client
        .utxo({
          outputReferences: [
            {
              index: transactionInputJson.index,
              transaction: { id: transactionInputJson.transaction_id },
            },
          ],
        })
        .then(ogmiosUtxoToCslUtxo);
      if (utxos.len() === 0) {
        return undefined;
      } else {
        return utxos.get(0);
      }
    } finally {
      client.shutdown();
    }
  }

  async submitTx(transaction: csl.Transaction): Promise<csl.TransactionHash> {
    const context = await this.createInteractionContext();
    const client = await createTransactionSubmissionClient(context);
    try {
      const txId = await client.submitTransaction(transaction.to_hex());
      return csl.TransactionHash.from_hex(txId);
    } finally {
      client.shutdown();
    }
  }

  async awaitTx(
    transactionHash: csl.TransactionHash,
    pollDelayMs = 1000,
    maxCount = 5,
  ): Promise<void> {
    const context = await this.createInteractionContext();
    const client = await createLedgerStateQueryClient(context);

    let count = 0;

    try {
      await new Promise<void>((resolve, reject) => {
        const poll = setInterval(() => {
          client
            .utxo({
              outputReferences: [
                {
                  // Use the fact that transactions always
                  // should have at least 1 transaction output,
                  // and (it seems!) transaction outputs start
                  // at 0.
                  index: 0,
                  transaction: { id: transactionHash.to_hex() },
                },
              ],
            })
            .then((utxos) => {
              if (utxos.length !== 0 || !(count < maxCount)) {
                clearInterval(poll);
                resolve();
              }
              ++count;
            })
            .catch(reject);
        }, pollDelayMs);
      });
    } finally {
      client.shutdown();
    }
  }
  /**
   * Queries the execution units of the transaction
   */
  async evaluateTx(
    transaction: csl.Transaction,
    excessBudgetFactor = 1.1,
  ): Promise<TxExUnits> {
    const context = await this.createInteractionContext();
    const client = await createTransactionSubmissionClient(context);
    try {
      const evalResults = await client.evaluateTransaction(
        transaction.to_hex(),
        // TODO(jaredponn): expose the API to allow users to provide
        // extra UTxOs when evaluating the transaction
        undefined,
      );

      return ogmiosEvaluateTransactionToCslExUnits(
        transaction,
        evalResults,
        excessBudgetFactor,
      );
    } finally {
      client.shutdown();
    }
  }

  async queryCostmdls(): Promise<csl.Costmdls> {
    const context = await this.createInteractionContext();
    const client = await createLedgerStateQueryClient(context);
    try {
      // Grab the protocol parameters
      // See the CSL docs: https://developers.cardano.org/docs/get-started/cardano-serialization-lib/generating-transactions
      const params = await client.protocolParameters();

      if (params.plutusCostModels === undefined) {
        throw new Error(`Ogmios missing plutusCostModels`);
      }

      const costmdls = csl.Costmdls.new();

      for (const [key, value] of Object.entries(params.plutusCostModels)) {
        if (key === "plutus:v1") {
          const costModel = csl.CostModel.new();
          for (let i = 0; i < value.length; ++i) {
            costModel.set(i, csl.Int.from_str(value[i]!.toString()));
          }

          costmdls.insert(csl.Language.new_plutus_v1(), costModel);
        } else if (key === "plutus:v2") {
          const costModel = csl.CostModel.new();
          for (let i = 0; i < value.length; ++i) {
            costModel.set(i, csl.Int.from_str(value[i]!.toString()));
          }

          costmdls.insert(csl.Language.new_plutus_v2(), costModel);
        } else if (key === "plutus:v3") {
          const costModel = csl.CostModel.new();
          for (let i = 0; i < value.length; ++i) {
            costModel.set(i, csl.Int.from_str(value[i]!.toString()));
          }

          costmdls.insert(csl.Language.new_plutus_v3(), costModel);
        } else {
          throw new Error(
            `Unsupported Plutus version when getting cost model ${key}`,
          );
        }
      }

      return costmdls;
    } finally {
      client.shutdown();
    }
  }
  /**
   * Queries the {@link TxBuilderConfig} whose fields may be obtained by the
   * Cardano node's protocol parameters
   */
  async queryTxBuilderConfig(): Promise<TxBuilderConfig> {
    const context = await this.createInteractionContext();
    const client = await createLedgerStateQueryClient(context);
    try {
      const params = await client.protocolParameters();
      return ogmiosProtocolParametersToTxBuilderConfig(params);
    } finally {
      client.shutdown();
    }
  }
}

/**
 * Translates the (successful) result of Ogmios' evaluate transaction into a
 * format compatible with {@link  TxBuilderEnv }
 *
 * @see {@link https://ogmios.dev/mini-protocols/local-tx-submission/#evaluating-transactions}
 */
function ogmiosEvaluateTransactionToCslExUnits(
  tx: csl.Transaction,
  resp: ogmios.EvaluateTransactionSuccess["result"],
  excessBudgetFactor = 1.1,
): TxExUnits {
  const inputs = tx.body().inputs();

  const result: TxExUnits = {};

  for (const i of resp) {
    const validatorPurpose = i.validator;

    const spendingInput = inputs.get(validatorPurpose.index);
    result[spendingInput.to_hex()] = csl.ExUnits.new(
      csl.BigNum.from_str(
        Math.ceil(i.budget.memory * excessBudgetFactor).toString(),
      ),
      csl.BigNum.from_str(
        Math.ceil(i.budget.cpu * excessBudgetFactor).toString(),
      ),
    );
  }

  return result;
}

function ogmiosProtocolParametersToTxBuilderConfig(
  params: ogmios.ProtocolParameters,
): TxBuilderConfig {
  // See {@link https://github.com/CardanoSolutions/ogmios/blob/v6.0.0/server/src/Ogmios/Data/Json/Alonzo.hs#L278-L281 }
  // for where they renamed
  //  - minFeeA --> minFeeCoefficient
  //  - minFeeB --> minFeeConstant
  const minFeeCoefficient = csl.BigNum.from_str(
    params.minFeeCoefficient.toString(),
  );

  if (params.minFeeConstant === undefined) {
    throw new Error(`Ogmios protocol parameters is missing minFeeConstant`);
  }

  const minFeeConstant = csl.BigNum.from_str(
    params.minFeeConstant.ada.lovelace.toString(),
  );

  const linearFee = csl.LinearFee.new(minFeeCoefficient, minFeeConstant);

  if (params.stakePoolDeposit === undefined) {
    throw new Error(`Ogmios protocol parameters is missing stakePoolDeposit`);
  }

  const stakePoolDeposit = csl.BigNum.from_str(
    params.stakePoolDeposit.ada.lovelace.toString(),
  );

  if (params.stakeCredentialDeposit === undefined) {
    throw new Error(
      `Ogmios protocol parameters is missing stakeCredentialDeposit`,
    );
  }

  const stakeCredentialDeposit = csl.BigNum.from_str(
    params.stakeCredentialDeposit.ada.lovelace.toString(),
  );

  if (params.maxValueSize === undefined) {
    throw new Error(`Ogmios protocol parameters is missing maxValueSize`);
  }

  const maxValueSize = params.maxValueSize.bytes;

  if (params.maxTransactionSize === undefined) {
    throw new Error(`Ogmios protocol parameters is missing maxTransactionSize`);
  }

  const maxTransactionSize = params.maxTransactionSize.bytes;

  /*
   * See the following snippets for where ogmios renamed
   *  - `coinsPerUtxoByte` ---> `minUtxoDepositCoefficient`
   * {@link https://github.com/CardanoSolutions/ogmios/blob/0c16007a936648ee7e924bb305eaa5c24464acff/server/src/Ogmios/Data/Json/Alonzo.hs#L178-L202}
   * *{@link https://cips.cardano.org/cip/CIP-0055}
   */
  if (params.minUtxoDepositCoefficient === undefined) {
    throw new Error(
      `Ogmios protocol parameters is missing minUtxoDepositCoefficient`,
    );
  }

  const coinsPerUtxoByte = csl.BigNum.from_str(
    params.minUtxoDepositCoefficient.toString(),
  );

  if (params.scriptExecutionPrices === undefined) {
    throw new Error(
      `Ogmios protocol parameters is missing scriptExecutionPrices`,
    );
  }

  const memPrice = rationalToUnitInterval(params.scriptExecutionPrices.memory);
  const cpuPrice = rationalToUnitInterval(params.scriptExecutionPrices.cpu);
  const exUnitPrices = csl.ExUnitPrices.new(memPrice, cpuPrice);

  return {
    feeAlgo: linearFee,
    // https://github.com/CardanoSolutions/ogmios/blob/v6.0.0/server/src/Ogmios/Data/Json/Alonzo.hs#L288-L291
    poolDeposit: stakePoolDeposit,
    // https://github.com/CardanoSolutions/ogmios/blob/v6.0.0/server/src/Ogmios/Data/Json/Alonzo.hs#L288-L291
    keyDeposit: stakeCredentialDeposit,
    maxValueSize: maxValueSize,
    maxTxSize: maxTransactionSize,
    coinsPerUtxoByte: coinsPerUtxoByte,
    exUnitPrices: exUnitPrices,
    preferPureChange: false,
  };
}

function rationalToUnitInterval(str: string) {
  const rationalRegex = /(^-?[0-9]+)\/([0-9]+$)/;
  const [_, num, den] = str.match(rationalRegex)!;
  return csl.UnitInterval.new(
    csl.BigNum.from_str(num!),
    csl.BigNum.from_str(den!),
  );
}
