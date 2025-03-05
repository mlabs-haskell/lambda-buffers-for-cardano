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
 * serialization lib's transaction building functionality.
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
  build(
    txBuilderConfig: TxBuilderConfig,
    exUnits?: TxExUnits,
  ): csl.Transaction {
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
    return this.then((env) => (env.txBuilder.add_output(txOut), env));
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
      (env) => (
        (env.txBuilder.add_inputs_from(inputs, strategy), env.txBuilder), env
      ),
    );
  }

  /**
   * Sets the inputs in the transaction to all inputs added with functions of
   * the form `add*Input`.
   *
   * Internally, we maintain a {@link csl.TxInputsBuilder} which is where all
   * functions `add*Input` adds the inputs to, and this {@link setInputs} will
   * set the transaction's inputs to the {@link csl.TxInputsBuilder} inputs.
   *
   * @remarks This should be called _before_ {@link addInputsFrom} as it
   * would otherwise overwrite the already existing inputs used to pay fees.
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
    return this.then((env) => (env.txBuilder.add_change_if_needed(addr), env));
  }

  /**
   * @see {@link https://github.com/Emurgo/cardano-serialization-lib/blob/11.5.0/rust/src/tx_builder.rs#L1711-L1720}
   */
  calcScriptDataHash(costmdl: csl.Costmdls) {
    return this.then(
      (env) => (env.txBuilder.calc_script_data_hash(costmdl), env),
    );
  }

  /**
   * Adds a Plutus Script to the inputs of the transaction
   * @param mkWitness - function to produce a {@link csl.PlutusWitness} from
   * the provided `tag`, `index`, and `exUnits`.
   * @param txInput - transaction input to spend
   * @param value - the value of the aforementioned transaction input
   *
   * @remarks
   * Because of internals of cardano-serialization-library, after one has added
   * all inputs which were not added by {@link addInputsFrom}, one must call
   * {@link setInputs}.
   *
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
    return this.then((env) => (env.txBuilder.set_collateral(collateral), env));
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
  const fixed_tx = csl.FixedTransaction.new_from_body_bytes(
    tx.body().to_bytes(),
  );
  const tx_hash = fixed_tx.transaction_hash();
  const vkeyWitness = csl.make_vkey_witness(tx_hash, sk);
  const witnesses = tx.witness_set();
  const vkeys =
    ((vkeys) => vkeys === undefined ? csl.Vkeywitnesses.new() : vkeys)(
      witnesses.vkeys(),
    );
  vkeys.add(vkeyWitness);
  witnesses.set_vkeys(vkeys);
  return csl.Transaction.new(fixed_tx.body(), witnesses, tx.auxiliary_data());
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
