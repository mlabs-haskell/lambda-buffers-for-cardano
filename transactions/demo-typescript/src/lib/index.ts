// deno-lint-ignore-file no-explicit-any
/**
 * This file includes code for creating the transactions for this demo project
 */
import * as LbrPlutus from "lbr-plutus/PlutusData.js";
import {
  EqDatum,
  EqRedeemer,
} from "lbf-demo-plutus-api/LambdaBuffers/Demo/Plutus.mjs";
// WARNING(jaredponn): the npm package released by @emurgo leaks memory
// everywhere. When one compiles the Rust code using wasm-bindgen, one must
// compile it with the flag `--weak-refs` s.t. their objects are automatically
// garbage collected by JS' runtime. @emurgo's npm package does not do this.
import * as csl from "@emurgo/cardano-serialization-lib-nodejs";
import { RtsConfig } from "./rtsconfig.js";
import { plaPlutusDataToCslPlutusData } from "./utils.js";

import { addVkeyWitness, TxBuilder } from "./tx-builder.js";
import { Query } from "./query.js";
import { Submit } from "./submit.js";
import { Ogmios } from "./ogmios.js";

// Patch {@link bigint} s.t. JSON.stringify can print it for debugging.
(BigInt.prototype as any).toJSON = function () {
  return this.toString();
};

/**
 * {@link Demo} is a class for querying UTxOs / building transactions related
 * to the demo protocol
 */
export class Demo {
  query: Query & Submit;
  #signingKey: csl.PrivateKey;
  #signingKeyAddress: csl.Address;

  /**
   * @internal
   */
  constructor(
    query: Query & Submit,
    signingKey: csl.PrivateKey,
    signingKeyAddress: csl.Address,
  ) {
    this.query = query;
    this.#signingKey = signingKey;
    this.#signingKeyAddress = signingKeyAddress;
  }

  /**
   * Creates a new {@link Demo} from the provided {@link RtsConfig}
   */
  static async new(rtsConfig: RtsConfig): Promise<Demo> {
    const query = await Ogmios.new(rtsConfig.ogmios);
    return new Demo(query, rtsConfig.signingKey, rtsConfig.signingKeyAddress);
  }

  /**
   * {@link createValueTx} creates a transaction which has a transaction
   * output at the provided `eqValidatorAddress` with the given `eqDatum`
   *
   * @returns A transaction satisfying the aforementioned properties
   */
  async createValueTx({
    eqValidatorAddress,
    eqDatum,
  }: {
    eqValidatorAddress: csl.Address;
    eqDatum: EqDatum;
  }): Promise<csl.Transaction> {
    const txBuilderConfig = await this.query.queryTxBuilderConfig();
    const sk = this.#signingKey;
    const skAddress = this.#signingKeyAddress;
    const changeAddress = this.#signingKeyAddress;
    const availableInputs = await this.query.queryAddressUtxos(skAddress);

    const eqDatumPd = plaPlutusDataToCslPlutusData(
      LbrPlutus.IsPlutusData[EqDatum].toData(eqDatum),
    );

    const txBuilder = TxBuilder.new()
      /*
       * Create the transaction output at `eqValidatorAddress` with datum
       * `eqDatum`
       */
      .addOutputWithDataCost(() =>
        csl.TransactionOutputBuilder.new()
          .with_address(eqValidatorAddress)
          .with_plutus_data(eqDatumPd)
          .next()
          .with_coin(csl.BigNum.from_str("2000000"))
          // FIXME: I had to hardcode the min UTxO because of a weird error: 'Total input and total output are not equal. Total input: {\n  "coin": "9000000000",\n  "multiasset": null\n}, Total output: {\n  "coin": "9000000000",\n  "multiasset": {}\n}'
          // .with_asset_and_min_required_coin_by_utxo_cost(
          //   csl.MultiAsset.new(),
          //   dataCost,
          // )
          .build()
      )
      /*
       * Add inputs to pay fees
       */
      .addInputsFrom(
        availableInputs,
        csl.CoinSelectionStrategyCIP2.RandomImproveMultiAsset,
      )
      /*
       * Add change
       */
      .addChangeIfNeeded(changeAddress);

    const unsignedTx = txBuilder.build(txBuilderConfig);
    const signedTx = addVkeyWitness(unsignedTx, sk);

    return signedTx;
  }

  /**
   * {@link inputIsEqualTx} makes a transaction that checks if the {@link
   * EqDatum} stored at the `eqValidator`'s `eqValidatorTxIn` IS EQUAL to the
   * provided one in `eqDatum`.
   *
   * @returns A transaction satisfying the aforementioned properties
   */
  async inputIsEqualTx({
    eqValidator,
    eqValidatorTxIn,
    eqDatum,
  }: {
    eqValidator: csl.PlutusScript;
    eqValidatorTxIn: csl.TransactionInput;
    eqDatum: EqDatum;
  }): Promise<csl.Transaction> {
    const tx = await this.inputValueTx({
      eqValidator,
      eqValidatorTxIn,
      eqRedeemer: { name: "IsEqual", fields: eqDatum },
    });
    return tx;
  }

  /**
   * {@link inputIsNotEqualTx} makes a transaction that checks if the {@link
   * EqDatum} stored at the `eqValidator`'s `eqValidatorTxIn` is NOT equal to the
   * provided one in `eqDatum`.
   *
   * @returns A transaction satisfying the aforementioned properties
   */
  async inputIsNotEqualTx({
    eqValidator,
    eqValidatorTxIn,
    eqDatum,
  }: {
    eqValidator: csl.PlutusScript;
    eqValidatorTxIn: csl.TransactionInput;
    eqDatum: EqDatum;
  }): Promise<csl.Transaction> {
    const tx = await this.inputValueTx({
      eqValidator,
      eqValidatorTxIn,
      eqRedeemer: { name: "IsNotEqual", fields: eqDatum },
    });
    return tx;
  }

  /**
   * Computes a transaction which spends an `eqValidatorAddress` at the provided
   * `eqValidatorTxIn` with the provided redeemer `eqRedeemer`
   *
   * @returns A transaction satisfying the aforementioned properties
   */
  async inputValueTx({
    eqValidator,
    eqValidatorTxIn,
    eqRedeemer,
  }: {
    eqValidator: csl.PlutusScript;
    eqValidatorTxIn: csl.TransactionInput;
    eqRedeemer: EqRedeemer;
  }): Promise<csl.Transaction> {
    const sk = this.#signingKey;
    const skAddress = this.#signingKeyAddress;
    const changeAddress = this.#signingKeyAddress;

    const availableInputs = await this.query.queryAddressUtxos(skAddress);
    const costModel = await this.query.queryCostmdls();
    const txBuilderConfig = await this.query.queryTxBuilderConfig();

    /*
     * Pick a collateral UTxO
     */
    const collateralInputsBuilder = csl.TxInputsBuilder.new();
    if (availableInputs.len() === 0) {
      throw new Error(`No inputs available for collateral`);
    }

    // TODO(jaredponn): we should choose the collateral
    // properly
    //  - properly calculate how much collateral we should add
    //  - pick UTxOs which only have ada (requirement from the ledger) or
    //    use the fancy new collateral features which relax this
    //    requirement
    collateralInputsBuilder.add_regular_input(
      skAddress,
      availableInputs.get(0).input(),
      availableInputs.get(0).output().amount(),
    );

    /*
     * Lookup the TransactionOutput which corresponds to `eqValidatorTxIn`.
     */
    const eqValidatorUtxo = await this.query.queryUtxo(eqValidatorTxIn);

    if (eqValidatorUtxo === undefined) {
      throw new Error(
        `No UTxO found for ${JSON.stringify(eqValidatorTxIn.to_js_value())}`,
      );
    }
    const eqValidatorTxOut = eqValidatorUtxo.output();
    const eqValidatorDatum = eqValidatorTxOut.plutus_data();

    if (eqValidatorDatum === undefined) {
      throw new Error(
        `No inline datum found for ${
          JSON.stringify(
            eqValidatorUtxo.to_js_value(),
          )
        }`,
      );
    }
    // NOTE(jaredponn): we don't need the datum since the UTxO has it stored
    // inline
    eqValidatorDatum;
    const eqValidatorRedeemerPd = plaPlutusDataToCslPlutusData(
      LbrPlutus.IsPlutusData[EqRedeemer].toData(eqRedeemer),
    );

    const txBuilder = TxBuilder.new()
      .addPlutusScriptInput(
        /*
         * Create the witness for `eqValidatorTxIn`
         */
        (tag, index, exUnits) => {
          const eqValidatorRedeemer = csl.Redeemer.new(
            tag,
            index,
            eqValidatorRedeemerPd,
            exUnits,
          );
          const eqValidatorWitness = csl.PlutusWitness.new_without_datum(
            eqValidator,
            eqValidatorRedeemer,
          );
          return eqValidatorWitness;
        },
        eqValidatorTxIn,
        eqValidatorTxOut.amount(),
      )
      /*
       * Add collateral
       */
      .setCollateral(collateralInputsBuilder)
      /*
       * Set the inputs
       */
      .setInputs()
      /*
       * Calculate the script integrity hash
       */
      .calcScriptDataHash(costModel)
      /*
       * Add inputs to pay fees
       */
      .addInputsFrom(
        availableInputs,
        csl.CoinSelectionStrategyCIP2.RandomImproveMultiAsset,
      )
      /*
       * Add change
       */
      .addChangeIfNeeded(changeAddress);

    const fakeTx = addVkeyWitness(txBuilder.build(txBuilderConfig), sk);
    const exUnits = await this.query.evaluateTx(fakeTx);
    const realTx = addVkeyWitness(
      txBuilder.build(txBuilderConfig, exUnits),
      sk,
    );

    return realTx;
  }
}
