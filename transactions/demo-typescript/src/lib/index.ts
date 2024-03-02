// deno-lint-ignore-file no-explicit-any

// This file includes code for creating the transactions
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
import { ogmiosCreateContext, rtsConfig } from "./rtsconfig.js";
import {
  createLedgerStateQueryClient,
  createTransactionSubmissionClient,
} from "@cardano-ogmios/client";
import type { ProtocolParameters } from "@cardano-ogmios/schema";

import {
  ogmiosUtxoToCslUtxo,
  ogmiosValueToCslValue,
  plaPlutusDataToCslPlutusData,
} from "./utils.js";

function rationalToUnitInterval(str: string) {
  const rationalRegex = /(^-?[0-9]+)\/([0-9]+$)/;
  const [_, num, den] = str.match(rationalRegex)!;
  return csl.UnitInterval.new(
    csl.BigNum.from_str(num!),
    csl.BigNum.from_str(den!),
  );
}

// Patch {@link BigInt} s.t. toJSON can print it for debugging.
(BigInt.prototype as any).toJSON = function () {
  return this.toString();
};

/**
 * Creates a {@link csl.TransactionBuilder} and returns the {@link
 * ProtocolParameters} from the data provided from ogmios.
 *
 * @returns A {@link csl.TransactionBuilder} with the protocol parameters from
 * ogmios + the protocol parameters from ogmios.
 */
export async function createTxBuilder(): Promise<
  {
    transactionBuilder: csl.TransactionBuilder;
    protocolParameters: ProtocolParameters;
  }
> {
  // Set up a connection to ogmios
  const context = await ogmiosCreateContext();
  const client = await createLedgerStateQueryClient(context);
  try {
    // Grab the protocol parameters
    // See the CSL docs: https://developers.cardano.org/docs/get-started/cardano-serialization-lib/generating-transactions
    const params = await client.protocolParameters();

    const minFeeCoefficient = csl.BigNum.from_str(
      params.minFeeCoefficient.toString(),
    );

    if (params.minFeeConstant === undefined) {
      throw new Error(`Ogmios protocol parameters is missing minFeeConstant`);
    }

    if ((params.minFeeConstant as any).lovelace === undefined) {
      throw new Error(
        `Internal error hacking around ogmios is no longer valid. See nearby TODO(jaredponn) in the source.`,
      );
    }

    const minFeeConstant = csl.BigNum.from_str(
      // TODO(jaredponn): surely this is a bug (or mismatching ogmios versions)
      // -- the types want this to be at `.ada.lovelace`, but the JSON object
      // returned from ogmios only has `.lovelace`.
      // Hence, why we cast everything to `any`.
      (params.minFeeConstant as any).lovelace.toString(),
    );

    const linearFee = csl.LinearFee.new(minFeeCoefficient, minFeeConstant);

    if (params.stakePoolDeposit === undefined) {
      throw new Error(`Ogmios protocol parameters is missing stakePoolDeposit`);
    }

    if ((params.stakePoolDeposit as any).lovelace === undefined) {
      throw new Error(
        `Internal error hacking around ogmios is no longer valid. See nearby TODO(jaredponn) in the source.`,
      );
    }

    const stakePoolDeposit = csl.BigNum.from_str(
      (params.stakePoolDeposit as any).lovelace.toString(),
    );

    if (params.stakeCredentialDeposit === undefined) {
      throw new Error(
        `Ogmios protocol parameters is missing stakeCredentialDeposit`,
      );
    }

    if ((params.stakeCredentialDeposit as any).lovelace === undefined) {
      throw new Error(
        `Internal error hacking around ogmios is no longer valid. See nearby TODO(jaredponn) in the source.`,
      );
    }

    const stakeCredentialDeposit = csl.BigNum.from_str(
      (params.stakeCredentialDeposit as any).lovelace.toString(),
    );

    if (params.maxValueSize === undefined) {
      throw new Error(`Ogmios protocol parameters is missing maxValueSize`);
    }

    const maxValueSize = params.maxValueSize.bytes;

    if (params.maxTransactionSize === undefined) {
      throw new Error(
        `Ogmios protocol parameters is missing maxTransactionSize`,
      );
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

    const memPrice = rationalToUnitInterval(
      params.scriptExecutionPrices.memory,
    );
    const cpuPrice = rationalToUnitInterval(params.scriptExecutionPrices.cpu);
    const exUnitPrices = csl.ExUnitPrices.new(memPrice, cpuPrice);

    const txBuilderCfg = csl.TransactionBuilderConfigBuilder.new()
      .fee_algo(linearFee)
      .pool_deposit(stakePoolDeposit)
      .key_deposit(stakeCredentialDeposit)
      .max_value_size(maxValueSize)
      .max_tx_size(maxTransactionSize)
      .coins_per_utxo_byte(coinsPerUtxoByte)
      .ex_unit_prices(exUnitPrices)
      .build();

    const txBuilder = csl.TransactionBuilder.new(txBuilderCfg);

    return { transactionBuilder: txBuilder, protocolParameters: params };
  } finally {
    client.shutdown();
  }
}

/**
 * {@link createValueTx} creates a transaction which has a transaction
 * output at the provided `eqValidatorAddress` with the given `eqDatum`
 *
 * @returns A transaction satisfying the aforementioned properties
 */
export async function createValueTx(
  { eqValidatorAddress, eqDatum }: {
    eqValidatorAddress: csl.Address;
    eqDatum: EqDatum;
  },
): Promise<csl.Transaction> {
  const sk = rtsConfig.signingKey;
  const skAddress = rtsConfig.signingKeyAddress;
  const changeAddress = rtsConfig.signingKeyAddress;
  const { transactionBuilder, protocolParameters } = await createTxBuilder();

  const eqDatumPd = plaPlutusDataToCslPlutusData(
    LbrPlutus.IsPlutusData[EqDatum].toData(eqDatum),
  );
  /*
   * Create the transaction output at `eqValidatorAddress` with datum
   * `eqDatum`
   */
  const txOut = csl.TransactionOutputBuilder
    .new()
    .with_address(eqValidatorAddress)
    .with_plutus_data(
      eqDatumPd,
    )
    .next()
    .with_asset_and_min_required_coin_by_utxo_cost(
      csl.MultiAsset.new(),
      csl.DataCost.new_coins_per_byte(
        csl.BigNum.from_str(
          protocolParameters.minUtxoDepositCoefficient.toString(),
        ),
      ),
    )
    .build();

  //  Add `txOut` to the transaction
  transactionBuilder.add_output(txOut);

  /*
   * Add inputs to pay fees
   */
  const availableInputs = await queryAddressUtxos(skAddress);

  transactionBuilder.add_inputs_from(
    availableInputs,
    csl.CoinSelectionStrategyCIP2.RandomImproveMultiAsset,
  );

  /*
   * Create the change outputs
   */
  transactionBuilder.add_change_if_needed(changeAddress);

  /*
   * Building the transaction
   */
  // Create the transaction body + the hash + the empty witness set
  const transactionBody = transactionBuilder.build();
  const transactionHash = csl.hash_transaction(transactionBody);
  const witnesses = csl.TransactionWitnessSet.new();

  // Create the vkey witness + put it in the witness set
  const vkeyWitnesses = csl.Vkeywitnesses.new();
  const vkeyWitness = csl.make_vkey_witness(transactionHash, sk);
  vkeyWitnesses.add(vkeyWitness);
  witnesses.set_vkeys(vkeyWitnesses);

  /*
   * Build the transaction
   */
  const transaction = csl.Transaction.new(
    transactionBody,
    witnesses,
    undefined,
  );

  return transaction;
}

/**
 * {@link inputIsEqualTx} makes a transaction that checks if the {@link
 * EqDatum} stored at the `eqValidator`'s `eqValidatorTxIn` IS EQUAL to the
 * provided one in `eqDatum`.
 *
 * @returns A transaction satisfying the aforementioned properties
 */
export async function inputIsEqualTx(
  { eqValidator, eqValidatorTxIn, eqDatum }: {
    eqValidator: csl.PlutusScript;
    eqValidatorTxIn: csl.TransactionInput;
    eqDatum: EqDatum;
  },
): Promise<csl.Transaction> {
  const tx = await inputValueTx({
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
export async function inputIsNotEqualTx(
  {
    eqValidator,
    eqValidatorTxIn,
    eqDatum,
  }: {
    eqValidator: csl.PlutusScript;
    eqValidatorTxIn: csl.TransactionInput;
    eqDatum: EqDatum;
  },
): Promise<csl.Transaction> {
  const tx = await inputValueTx({
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
export async function inputValueTx(
  { eqValidator, eqValidatorTxIn, eqRedeemer }: {
    eqValidator: csl.PlutusScript;
    eqValidatorTxIn: csl.TransactionInput;
    eqRedeemer: EqRedeemer;
  },
): Promise<csl.Transaction> {
  const sk = rtsConfig.signingKey;
  const skAddress = rtsConfig.signingKeyAddress;
  const changeAddress = rtsConfig.signingKeyAddress;

  const availableInputs = await queryAddressUtxos(skAddress);
  const costModel = await queryCostmdls();

  const context = await ogmiosCreateContext();
  const client = await createLedgerStateQueryClient(context);

  try {
    /*
     * The steps are as follows:
     *
     *  - We create a "fake" transaction as close as possible to the real
     *  transaction, submit it to ogmios to compute the costs of running the script
     *
     *  - Once we have the costs of the scripts from the previous step, we
     *  rebuild the transaction with those costs and return that.
     */

    /*
     * Lookup the TransactionOutput which corresponds to `eqValidatorTxIn`.
     */
    const eqValidatorTxInJson: csl.TransactionInputJSON = eqValidatorTxIn
      .to_js_value();

    const eqValidatorTxOuts = await client.utxo({
      outputReferences: [{
        index: eqValidatorTxInJson.index,
        transaction: { id: eqValidatorTxInJson.transaction_id },
      }],
    });

    if (eqValidatorTxOuts.length !== 1) {
      throw new Error(
        `More than one UTxO exists for transaction input: ${
          JSON.stringify(eqValidatorTxInJson)
        }`,
      );
    }

    const eqValidatorTxOut = eqValidatorTxOuts[0]!;
    if (eqValidatorTxOut.datum === undefined) {
      throw new Error(
        `No inline datum found for ${JSON.stringify(eqValidatorTxInJson)}`,
      );
    }
    // NOTE(jaredponn): we don't need the datum since the UTxO has it stored
    // inline
    const eqValidatorDatum = csl.PlutusData.from_hex(eqValidatorTxOut.datum);
    eqValidatorDatum;
    const eqValidatorRedeemerPd = plaPlutusDataToCslPlutusData(
      LbrPlutus.IsPlutusData[EqRedeemer].toData(eqRedeemer),
    );

    /*
     * Build the "fake" transaction and grab the execution costs
     */
    const { memory, cpu } = await (async () => {
      /*
       * Create the witness for `eqValidatorTxIn`
       */
      const eqValidatorRedeemer = csl.Redeemer.new(
        csl.RedeemerTag.new_spend(),
        csl.BigNum.from_str("0"), // this will be automatically reassigned to the correct index by cardano-serialization-lib
        eqValidatorRedeemerPd,
        csl.ExUnits.new(csl.BigNum.from_str("0"), csl.BigNum.from_str("0")),
        // Fill these with dummy values
      );
      const eqValidatorWitness = csl.PlutusWitness.new_without_datum(
        eqValidator,
        eqValidatorRedeemer,
      );

      const { transactionBuilder } = await createTxBuilder();

      transactionBuilder.add_plutus_script_input(
        eqValidatorWitness,
        eqValidatorTxIn,
        ogmiosValueToCslValue(eqValidatorTxOut.value),
      );

      /*
       * Add collateral
       */
      {
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
        collateralInputsBuilder.add_input(
          skAddress,
          availableInputs.get(0).input(),
          availableInputs.get(0).output().amount(),
        );

        transactionBuilder.set_collateral(collateralInputsBuilder);
      }

      /*
       * Add inputs to pay fees
       */
      transactionBuilder.add_inputs_from(
        availableInputs,
        csl.CoinSelectionStrategyCIP2.RandomImproveMultiAsset,
      );

      /*
       * Create the change outputs
       */
      transactionBuilder.add_change_if_needed(changeAddress);

      /*
       * Building the transaction
       */
      // Create the transaction body + the hash + the empty witness set
      const transactionBody = transactionBuilder.build();
      const transactionHash = csl.hash_transaction(transactionBody);
      const witnesses = csl.TransactionWitnessSet.new();

      // Create the witnesses put it in the witness set
      const redeemers = csl.Redeemers.new();
      const vkeyWitnesses = csl.Vkeywitnesses.new();
      const plutusScripts = csl.PlutusScripts.new();

      vkeyWitnesses.add(csl.make_vkey_witness(transactionHash, sk));
      redeemers.add(eqValidatorRedeemer);
      plutusScripts.add(eqValidator);

      witnesses.set_vkeys(vkeyWitnesses);
      witnesses.set_redeemers(redeemers);
      witnesses.set_plutus_scripts(plutusScripts);

      /*
       * Build the transaction
       */
      const transaction = csl.Transaction.new(
        transactionBody,
        witnesses,
        undefined,
      );

      /*
       * Evaluate the transaction
       */
      try {
        const evalClient = await createTransactionSubmissionClient(context);
        const evalResults = await evalClient.evaluateTransaction(
          transaction.to_hex(),
          undefined,
        );
        if (evalResults.length !== 1) {
          throw new Error(
            `Invalid evaluation. Expected exactly one Plutus script`,
          );
        }

        const evalResult = evalResults[0]!;
        // See the warning in:
        // https://ogmios.dev/mini-protocols/local-tx-submission/#evaluating-transactions
        // for why we increase the costs by 0.05
        return {
          memory: Math.ceil(evalResult.budget.memory * 1.05),
          cpu: Math.ceil(evalResult.budget.cpu * 1.05),
        };
      } catch (e) {
        // rethrow the error stringified s.t. it's easier to debug.
        throw new Error(JSON.stringify(e));
      }
    })();

    /*
     * Build the transaction for real with the costs from the previous
     * transaction
     * WARNING(jaredponn): this is mostly duplicated code from the fake
     * transaction above.
     */

    /*
     * Create the witness for `eqValidatorTxIn`
     */
    const eqValidatorRedeemer = csl.Redeemer.new(
      csl.RedeemerTag.new_spend(),
      csl.BigNum.from_str("0"), // this will be automatically reassigned to the correct index by cardano-serialization-lib
      eqValidatorRedeemerPd,
      csl.ExUnits.new(
        csl.BigNum.from_str(memory.toString()),
        csl.BigNum.from_str(cpu.toString()),
      ),
    );

    const eqValidatorWitness = csl.PlutusWitness.new_without_datum(
      eqValidator,
      eqValidatorRedeemer,
    );

    const { transactionBuilder } = await createTxBuilder();

    /*
     * Add tx inputs
     */
    {
      const txInputsBuilder = csl.TxInputsBuilder.new();
      txInputsBuilder.add_plutus_script_input(
        eqValidatorWitness,
        eqValidatorTxIn,
        ogmiosValueToCslValue(eqValidatorTxOut.value),
      );
      transactionBuilder.set_inputs(txInputsBuilder);
    }

    /*
     * Add collateral
     */
    {
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
      collateralInputsBuilder.add_input(
        skAddress,
        availableInputs.get(0).input(),
        availableInputs.get(0).output().amount(),
      );

      transactionBuilder.set_collateral(collateralInputsBuilder);
    }

    /*
     * Add inputs to pay fees
     */
    transactionBuilder.add_inputs_from(
      availableInputs,
      csl.CoinSelectionStrategyCIP2.RandomImproveMultiAsset,
    );

    /*
     * Calculate the script integrity hash
     */
    transactionBuilder.calc_script_data_hash(costModel);

    /*
     * Create the change outputs
     */
    transactionBuilder.add_change_if_needed(changeAddress);

    /*
     * Building the transaction / witness set
     */
    const redeemers = csl.Redeemers.new();
    const plutusScripts = csl.PlutusScripts.new();

    redeemers.add(eqValidatorRedeemer);
    plutusScripts.add(eqValidator);

    const witnesses = csl.TransactionWitnessSet.new();
    witnesses.set_redeemers(redeemers);
    witnesses.set_plutus_scripts(plutusScripts);

    const transactionBody = transactionBuilder.build();
    const transactionHash = csl.hash_transaction(transactionBody);

    const vkeyWitnesses = csl.Vkeywitnesses.new();
    vkeyWitnesses.add(csl.make_vkey_witness(transactionHash, sk));
    witnesses.set_vkeys(vkeyWitnesses);

    /*
     * Build the transaction
     */
    const transaction = csl.Transaction.new(
      transactionBody,
      witnesses,
      undefined,
    );

    return transaction;
  } finally {
    client.shutdown();
  }
}

/**
 * {@link queryAddressUtxos} queries all the UTxOs at the provided address from
 * ogmios
 *
 * @returns the UTxOs at the provided address
 */
export async function queryAddressUtxos(
  addr: csl.Address,
): Promise<csl.TransactionUnspentOutputs> {
  const context = await ogmiosCreateContext();
  const client = await createLedgerStateQueryClient(context);
  try {
    const utxos = await client.utxo({ addresses: [addr.to_bech32()] }).then(
      ogmiosUtxoToCslUtxo,
    );
    return utxos;
  } finally {
    client.shutdown();
  }
}

/**
 * {@link submitTransaction} submits the transaction to the Cardano node via
 * ogmios.
 *
 * @returns The transaction hash of the submitted transaction
 */
export async function submitTransaction(
  transaction: csl.Transaction,
): Promise<csl.TransactionHash> {
  const context = await ogmiosCreateContext();
  const client = await createTransactionSubmissionClient(context);

  try {
    const txId = await client.submitTransaction(transaction.to_hex());
    return csl.TransactionHash.from_hex(txId);
  } finally {
    client.shutdown();
  }
}

/**
 * Waits for a transaction (which is assumed to already submitted) until one
 * may query it from the ledger state; or until `maxCount * pollDelayMs` has passed.
 *
 * @remarks
 * - It is possible for a transaction to have 0 transaction outputs, and hence
 *   it may never be the case that the transaction produces outputs which may
 *   be queried. As such, this will alternatively return if (at least) `5 *
 *   pollDelayMs` has passed.
 *
 * For details on differing levels of transaction confirmation, see {@link
 * https://docs.cardano.org/learn/chain-confirmation-versus-transaction-confirmation/},
 * and one can implement a more specialized solution
 */
export async function awaitTransaction(
  transactionHash: csl.TransactionHash,
  pollDelayMs = 1000,
  maxCount = 5,
): Promise<void> {
  const context = await ogmiosCreateContext();
  const client = await createLedgerStateQueryClient(context);

  let count = 0;

  try {
    await new Promise<void>((resolve, reject) => {
      const poll = setInterval(
        () => {
          client.utxo({
            outputReferences: [{
              // Use the fact that transactions always
              // should have at least 1 transaction output,
              // and (it seems!) transaction outputs start
              // at 0.
              index: 0,
              transaction: { id: transactionHash.to_hex() },
            }],
          }).then((utxos) => {
            if (utxos.length !== 0 || !(count < maxCount)) {
              clearInterval(poll);
              resolve();
            }
            ++count;
          }).catch(reject);
        },
        pollDelayMs,
      );
    });
  } finally {
    client.shutdown();
  }
}

/**
 * {@link queryCostmdls} queries cost models from ogmios
 *
 * @returns {@link csl.Costmdls} filled in with the information from ogmios.
 */
export async function queryCostmdls(): Promise<csl.Costmdls> {
  const context = await ogmiosCreateContext();
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
