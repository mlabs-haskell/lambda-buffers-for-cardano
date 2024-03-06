/**
 * Functionality to isolate how we query the Cardano node
 */
import * as csl from "@emurgo/cardano-serialization-lib-nodejs";
import { TxBuilderConfig } from "./tx-builder.js";

/**
 * {@link Query} describes functionality for interaction with a Cardano
 * node to make this demo project work
 */
export interface Query {
  queryAddressUtxos(addr: csl.Address): Promise<csl.TransactionUnspentOutputs>;
  queryUtxo(
    transactionInput: csl.TransactionInput,
  ): Promise<csl.TransactionUnspentOutput | undefined>;

  queryCostmdls(): Promise<csl.Costmdls>;
  /**
   * Queries the {@link TxBuilderConfig} whose fields may be obtained by the
   * Cardano node's protocol parameters
   */
  queryTxBuilderConfig(): Promise<TxBuilderConfig>;
}
