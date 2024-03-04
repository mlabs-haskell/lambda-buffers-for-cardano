/**
 * Functionality for helping build transactions with cardano serialization lib
 */

import * as csl from "@emurgo/cardano-serialization-lib-nodejs";

/**
 * {@link TxExUnits} maps from hex encoded Plutus things (e.g. transaction
 * inputs) to the corresponding {@link csl.ExUnits} to execute it.
 */
export interface TxExUnits {
  [key: string]: csl.ExUnits;
}

/**
 * Corresponds to
 *  - {@link https://github.com/Emurgo/cardano-serialization-lib/blob/11.5.0/rust/src/tx_builder.rs#L212-L223}
 *  - {@link https://github.com/Emurgo/cardano-serialization-lib/blob/11.5.0/rust/src/tx_builder.rs#L245}
 */
export interface TxBuilderConfig {
  feeAlgo: csl.LinearFee;
  poolDeposit: csl.BigNum;
  keyDeposit: csl.BigNum;
  maxValueSize: number;
  maxTxSize: number;
  coinsPerUtxoByte: csl.BigNum;
  exUnitPrices: csl.ExUnitPrices;
  preferPureChange: boolean;
}

/**
 * Internal environment used when building the transaction
 *
 * @internal
 */
export interface TxBuilderEnv {
  txBuilder: csl.TransactionBuilder;
  txInputsBuilder: csl.TxInputsBuilder;
  txBuilderConfig: TxBuilderConfig;
  exUnits?: TxExUnits | undefined;
}

/**
 * {@link TxBuilder} is a thin wrapper around (a subset of) Cardano
 * serialization lib's transaction building functionality, but provides the
 * following additional functionality:
 *
 * - Evaluating transactions with ogmios to calculate fees for redeemers.
 *   Indeed, scripts must provide the {@link csl.ExUnits} required to submit,
 *   so in order to get this information, one needs to evaluate the script.
 *   Ogmios allows us to do this with {@link
 *   https://ogmios.dev/mini-protocols/local-tx-submission/}, where this {@link
 *   TxBuilder} will
 *
 *      - Create a "dummy" transaction
 */
export class TxBuilder {
  runTxBuilderEnv: (env: TxBuilderEnv) => TxBuilderEnv;

  constructor(f: (env: TxBuilderEnv) => TxBuilderEnv) {
    this.runTxBuilderEnv = f;
  }

  static new() {
    return new TxBuilder((env) => env);
  }

  /**
   * Builds a transaction with the provided protocol parameters
   */
  // buildPp(pp : ogmios.ProtocolParameters, exUnits? : { [key: string ]: csl.ExUnits } ) : csl.Transaction {
  build(
    txBuilderConfig: TxBuilderConfig,
    exUnits?: TxExUnits,
  ): csl.Transaction {
    // const cslTxBuilder = ogmiosProtocolParametersToCslTransactionBuilder(pp)

    const cslTxBuilder = txBuilderConfigToCslTransactionBuilder(
      txBuilderConfig,
    );

    const finalEnv = this.runTxBuilderEnv({
      txBuilder: cslTxBuilder,
      txInputsBuilder: csl.TxInputsBuilder.new(),
      txBuilderConfig: txBuilderConfig,
      exUnits,
    });
    const tx = finalEnv.txBuilder.build_tx();
    return tx;
  }

  /**
   * Wrapper around {@link csl.TransactionBuilder.add_output }
   */
  addOutput(txOut: csl.TransactionOutput): TxBuilder {
    return this.then((env) => ((env.txBuilder.add_output(txOut)), env));
  }

  /**
   * Wrapper around {@link csl.TransactionBuilder.add_output }
   */
  addOutputWithDataCost(
    f: (dataCost: csl.DataCost) => csl.TransactionOutput,
  ): TxBuilder {
    return this.then((env) => {
      const dataCost = txBuilderConfigToCslDataCost(env.txBuilderConfig);
      env.txBuilder.add_output(f(dataCost));
      return env;
    });
  }

  /**
   * @see {@link https://github.com/Emurgo/cardano-serialization-lib/blob/11.5.0/rust/src/tx_builder.rs#L369-L376 }
   */
  addInputsFrom(
    inputs: csl.TransactionUnspentOutputs,
    strategy: csl.CoinSelectionStrategyCIP2,
  ): TxBuilder {
    return this.then(
      (
        env,
      ) => ((env.txBuilder.add_inputs_from(inputs, strategy), env.txBuilder),
        env),
    );
  }

  /**
   * Sets the inputs to the inputs in the transaction to the inputs currently
   * added with functions `add*Input`.
   *
   * @remarks This should be called _before_ {@link addInputsFrom} as it
   * would otherwise overwrite the  already existing inputs.
   *
   * @see {@link https://github.com/Emurgo/cardano-serialization-lib/blob/11.5.0/rust/src/tx_builder.rs#L687-L689}
   */
  setInputs(): TxBuilder {
    return this.then(
      (env) => (env.txBuilder.set_inputs(env.txInputsBuilder), env),
    );
  }

  /**
   * Adds the change (after paying fees) to the provided {@link csl.Address}.
   * This mutates the transaction's fee, and hence must be called *last*
   * after setting all other tx-body properties such as inputs, outputs,
   * mint, script hashes, etc.
   *
   * @see {@link https://github.com/Emurgo/cardano-serialization-lib/blob/11.5.0/rust/src/tx_builder.rs#L1336-L1340}
   */
  addChangeIfNeeded(addr: csl.Address) {
    return this.then(
      (env) => ((env.txBuilder.add_change_if_needed(addr)), env),
    );
  }

  /**
   * @see {@link https://github.com/Emurgo/cardano-serialization-lib/blob/11.5.0/rust/src/tx_builder.rs#L1711-L1720}
   */
  calcScriptDataHash(costmdl: csl.Costmdls) {
    return this.then(
      (env) => ((env.txBuilder.calc_script_data_hash(costmdl)), env),
    );
  }

  /**
   * Adds a Plutus Script to the inputs of the transaction where one must
   *  - Provide the witness using the `tag`, `index`, and `exUnits` provided
   *    by the function. This is necessary to support evaluating the
   *    transaction figure out the ExUnits cost, then creating the
   *    transaction with the actual ExUnits cost
   *  - Provide the TransactionInput to spend
   *  - Provide the value at the TransactionInput we are spending
   * @see {@link https://github.com/Emurgo/cardano-serialization-lib/blob/master/rust/src/tx_builder/tx_inputs_builder.rs#L156-L168}
   */
  addPlutusScriptInput(
    mkWitness: (
      tag: csl.RedeemerTag,
      index: csl.BigNum,
      exUnits: csl.ExUnits,
    ) => csl.PlutusWitness,
    txInput: csl.TransactionInput,
    value: csl.Value,
  ) {
    return this.then((env) => {
      const witness = (() => {
        if (env.exUnits === undefined) {
          return mkWitness(
            csl.RedeemerTag.new_spend(),
            csl.BigNum.from_str("0"), // this will be automatically reassigned by csl
            csl.ExUnits.new(
              csl.BigNum.from_str((0).toString()),
              csl.BigNum.from_str((0).toString()),
            ), // fill this with dummy 0 values
          );
        } else {
          const lkup = env.exUnits[txInput.to_hex()];

          if (lkup === undefined) {
            throw new Error(
              `Transaction input is missing ExUnits from provided ExUnits: ${txInput.to_json()}`,
            );
          }
          return mkWitness(
            csl.RedeemerTag.new_spend(),
            csl.BigNum.from_str("0"), // this will be automatically reassigned by csl
            lkup,
          );
        }
      })();
      env.txInputsBuilder.add_plutus_script_input(witness, txInput, value);
      return env;
    });
  }

  /**
   * @see {@link  https://github.com/Emurgo/cardano-serialization-lib/blob/11.5.0/rust/src/tx_builder.rs#L691 }
   */
  setCollateral(collateral: csl.TxInputsBuilder): TxBuilder {
    return this.then(
      (env) => ((env.txBuilder.set_collateral(collateral)), env),
    );
  }

  /**
   * @example
   * ```ts
   * txBuilder.compose(otherTxBuilder)
   * ```
   */
  compose(txBuilder: TxBuilder): TxBuilder {
    return new TxBuilder((env) =>
      txBuilder.runTxBuilderEnv(this.runTxBuilderEnv(env))
    );
  }

  /**
   * @internal
   */
  then(f: (env: TxBuilderEnv) => TxBuilderEnv): TxBuilder {
    return new TxBuilder((env) => f(this.runTxBuilderEnv(env)));
  }
}

/**
 * Adds a verification key witness (signs the transaction)
 */
export function addVkeyWitness(
  tx: csl.Transaction,
  sk: csl.PrivateKey,
): csl.Transaction {
  const vkeyWitness = csl.make_vkey_witness(
    csl.hash_transaction(tx.body()),
    sk,
  );
  const witnesses = tx.witness_set();
  const vkeys =
    ((vkeys) => vkeys === undefined ? csl.Vkeywitnesses.new() : vkeys)(
      witnesses.vkeys(),
    );
  vkeys.add(vkeyWitness);
  witnesses.set_vkeys(vkeys);
  return csl.Transaction.new(tx.body(), witnesses, tx.auxiliary_data());
}

function txBuilderConfigToCslDataCost(conf: TxBuilderConfig): csl.DataCost {
  return csl.DataCost.new_coins_per_byte(conf.coinsPerUtxoByte);
}

function txBuilderConfigToCslTransactionBuilder(
  conf: TxBuilderConfig,
): csl.TransactionBuilder {
  const txBuilderCfg = csl.TransactionBuilderConfigBuilder.new()
    .fee_algo(conf.feeAlgo)
    .pool_deposit(conf.poolDeposit)
    .key_deposit(conf.keyDeposit)
    .max_value_size(conf.maxValueSize)
    .max_tx_size(conf.maxTxSize)
    .coins_per_utxo_byte(conf.coinsPerUtxoByte)
    .ex_unit_prices(conf.exUnitPrices)
    .build();

  const txBuilder = csl.TransactionBuilder.new(txBuilderCfg);

  return txBuilder;
}
