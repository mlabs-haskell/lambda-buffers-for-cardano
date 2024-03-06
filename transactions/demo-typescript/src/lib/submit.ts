/**
 * Functionality to isolate how we submit transactions the Cardano node
 */

import * as csl from "@emurgo/cardano-serialization-lib-nodejs";
import { TxExUnits } from "./tx-builder.js";

/**
 * {@link Submit} describes functionality for interaction with a Cardano
 * node which submits or waits for a transaction
 */
export interface Submit {
  submitTx(transaction: csl.Transaction): Promise<csl.TransactionHash>;
  awaitTx(
    transactionHash: csl.TransactionHash,
    pollDelayMs?: number,
    maxCount?: number,
  ): Promise<void>;
  /**
   * Queries the execution units of the transaction.
   *
   * @param excessBudgetFactor - what to multiply the budgets by after
   * evaluating it. See the warning {@link https://ogmios.dev/mini-protocols/local-tx-submission/#evaluating-transactions}
   */
  evaluateTx(
    transaction: csl.Transaction,
    excessBudgetFactor?: number,
  ): Promise<TxExUnits>;
}
