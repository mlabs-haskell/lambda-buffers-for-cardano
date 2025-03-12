// This file includes helpful utilities for e.g.
//
//  - translating plutus-ledger-api types to cardano-serialization-lib types
import * as Pla from "plutus-ledger-api/PlutusData.js";
import * as csl from "@emurgo/cardano-serialization-lib-nodejs";
import type { Utxo, Value } from "@cardano-ogmios/schema";

/**
 * Converts `plutus-ledger-api`'s {@link Pla.PlutusData} to
 * `cardano-serialization-lib`'s {@link csl.PlutusData}
 */
export function plaPlutusDataToCslPlutusData(
  plutusData: Pla.PlutusData,
): csl.PlutusData {
  switch (plutusData.name) {
    case "Integer":
      return csl.PlutusData.new_integer(
        csl.BigInt.from_str(plutusData.fields.toString()),
      );
    case "Bytes":
      return csl.PlutusData.new_bytes(plutusData.fields);
    case "List":
      return csl.PlutusData.new_list(
        plaPdListToCslPlutusList(plutusData.fields),
      );
    case "Constr":
      return csl.PlutusData.new_constr_plutus_data(
        csl.ConstrPlutusData.new(
          csl.BigNum.from_str(plutusData.fields[0].toString()),
          plaPdListToCslPlutusList(plutusData.fields[1]),
        ),
      );
    case "Map": {
      const plutusMap = csl.PlutusMap.new();
      for (const elem of plutusData.fields) {
        const plutusMapValues = csl.PlutusMapValues.new();
        plutusMapValues.add(plaPlutusDataToCslPlutusData(elem[1]));
        plutusMap.insert(
          plaPlutusDataToCslPlutusData(elem[0]),
          plutusMapValues,
        );
      }
      return csl.PlutusData.new_map(plutusMap);
    }
  }
}

/**
 * Converts `cardano-serialization-lib`'s {@link csl.PlutusData} to
 * `plutus-ledger-api`'s {@link Pla.PlutusData}.
 */
export function cslPlutusDataToPlaPlutusData(
  plutusData: csl.PlutusData,
): Pla.PlutusData {
  const constr = plutusData.as_constr_plutus_data();
  const map = plutusData.as_map();
  const list = plutusData.as_list();
  const integer = plutusData.as_integer();
  const bytes = plutusData.as_bytes();

  if (constr !== undefined) {
    const alternative = constr.alternative();
    const data = constr.data();

    return {
      name: "Constr",
      fields: [BigInt(alternative.to_str()), cslPlutusListToPlaPdList(data)],
    };
  }

  if (map !== undefined) {
    const keys = map.keys();
    const result: [Pla.PlutusData, Pla.PlutusData][] = [];

    for (let i = 0; i < keys.len(); ++i) {
      const k = keys.get(i);
      result.push([
        cslPlutusDataToPlaPlutusData(k),
        cslPlutusDataToPlaPlutusData(map.get(k)!.get(0)!),
      ]);
    }

    return { name: `Map`, fields: result };
  }

  if (list !== undefined) {
    return { name: `List`, fields: cslPlutusListToPlaPdList(list) };
  }

  if (integer !== undefined) {
    return { name: `Integer`, fields: BigInt(integer.to_str()) };
  }

  if (bytes !== undefined) {
    return { name: `Bytes`, fields: bytes };
  }

  throw new Error(
    "Internal error when converting cardano-serialization-lib PlutusData to plutus-ledger-api PlutusData",
  );
}

/**
 * Translates an {@link Value} from ogmios to a {@link csl.Value} from
 * `cardano-serialization-lib`
 */
export function ogmiosValueToCslValue(value: Value): csl.Value {
  const coin = csl.BigNum.from_str(value.ada.lovelace.toString());

  const multiasset = csl.MultiAsset.new();

  for (const [currencySymbol, tokenNameAmounts] of Object.entries(value)) {
    if (currencySymbol === "ada") continue;

    const scripthash = csl.ScriptHash.from_hex(currencySymbol);
    const assets = csl.Assets.new();

    for (const [tokenName, amount] of Object.entries(tokenNameAmounts)) {
      const assetName = csl.AssetName.from_hex(tokenName);
      const v = csl.BigNum.from_str(amount.toString());

      assets.insert(assetName, v);
    }
    multiasset.insert(scripthash, assets);
  }

  return csl.Value.new_with_assets(coin, multiasset);
}

/**
 * Converts an {@link Utxo} from ogmios (which is actually an array of UTxOs)
 * to an cardano-serialization-lib UTxOs.
 */
export function ogmiosUtxoToCslUtxo(
  utxos: Utxo,
): csl.TransactionUnspentOutputs {
  const result: csl.TransactionUnspentOutputs = csl.TransactionUnspentOutputs
    .new();

  for (const utxo of utxos) {
    const txId = csl.TransactionHash.from_hex(utxo.transaction.id);
    const index = utxo.index;

    const transactionInput = csl.TransactionInput.new(txId, index);

    const transactionOutput = csl.TransactionOutput.new(
      csl.Address.from_bech32(utxo.address), // TODO(jaredponn): addresses can also be in base58 format
      ogmiosValueToCslValue(utxo.value),
    );

    if (utxo.datumHash !== undefined) {
      transactionOutput.set_data_hash(csl.DataHash.from_hex(utxo.datumHash));
    }

    if (utxo.datum !== undefined) {
      transactionOutput.set_plutus_data(csl.PlutusData.from_hex(utxo.datum));
    }

    if (utxo.script !== undefined) {
      if (utxo.script.language === "native") {
        // TODO(jaredponn): just deserialize the cbor instead
        transactionOutput.set_script_ref(
          csl.ScriptRef.new_native_script(
            csl.NativeScript.from_json(JSON.stringify(utxo.script.json)),
          ),
        );
      } else if (utxo.script.language === "plutus:v1") {
        transactionOutput.set_script_ref(
          csl.ScriptRef.new_plutus_script(
            csl.PlutusScript.from_hex_with_version(
              utxo.script.cbor,
              csl.Language.new_plutus_v1(),
            ),
          ),
        );
      } else if (utxo.script.language === "plutus:v2") {
        transactionOutput.set_script_ref(
          csl.ScriptRef.new_plutus_script(
            csl.PlutusScript.from_hex_with_version(
              utxo.script.cbor,
              csl.Language.new_plutus_v2(),
            ),
          ),
        );
      } else if (utxo.script.language === "plutus:v3") {
        transactionOutput.set_script_ref(
          csl.ScriptRef.new_plutus_script(
            csl.PlutusScript.from_hex_with_version(
              utxo.script.cbor,
              csl.Language.new_plutus_v3(),
            ),
          ),
        );
      }
    }

    const transactionUnspentOutput = csl.TransactionUnspentOutput.new(
      transactionInput,
      transactionOutput,
    );

    result.add(transactionUnspentOutput);
  }

  return result;
}

/**
 * Translates the hex encoded cbor of a private key as outputted from
 * `cardano-cli` in the `cborHex` field to `cardano-serialization-lib`'s {@link
 * csl.PrivateKey}
 *
 * @example
 * As input, this should take the value of the `cborHex` key.
 * ```
 * {
 *     "type": "PaymentSigningKeyShelley_ed25519",
 *     "description": "Payment Signing Key",
 *     "cborHex": "5820cdb9d333ea48021d7c74852d6411f8c253ff266f95b23d5da1352a530b4e1bb8"
 * }
 * ```
 */
export function cborHexPrivateKey(cborHex: string) {
  const prvKeyPlutusData = csl.PlutusData.from_hex(cborHex);
  const prvKeyBytes = prvKeyPlutusData.as_bytes();
  if (prvKeyBytes === undefined) {
    throw new Error(`Invalid secret key`);
  }
  return csl.PrivateKey.from_normal_bytes(prvKeyBytes);
}

/**
 * @internal
 */
function plaPdListToCslPlutusList(list: Pla.PlutusData[]): csl.PlutusList {
  const result = csl.PlutusList.new();

  for (const elem of list) {
    result.add(plaPlutusDataToCslPlutusData(elem));
  }
  return result;
}

/**
 * @internal
 */
function cslPlutusListToPlaPdList(list: csl.PlutusList): Pla.PlutusData[] {
  const result = [];
  for (let i = 0; i < list.len(); ++i) {
    result.push(cslPlutusDataToPlaPlutusData(list.get(i)));
  }
  return result;
}
