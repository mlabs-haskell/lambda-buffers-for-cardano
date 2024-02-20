import { after, before, describe, it } from "node:test";
import * as DemoConfig from "lbf-demo-config-api/LambdaBuffers/Demo/Config.mjs";
import * as LbrPrelude from "lbr-prelude";
import * as LbrPlutusV1 from "lbr-plutus/V1.js";
import * as Prelude from "prelude";
import * as PlaV1 from "plutus-ledger-api/V1.js";
import * as PlaPd from "plutus-ledger-api/PlutusData.js";
import * as PreludeJson from "prelude/Json.js";
import { EqDatum } from "lbf-demo-plutus-api/LambdaBuffers/Demo/Plutus.mjs";
import { ChildProcess, spawn } from "node:child_process";
import { cslPlutusDataToPlaPlutusData } from "../lib/utils.js";
import { initRtsConfig } from "../lib/rtsconfig.js";
import {
  awaitTransaction,
  createValueTx,
  inputIsEqualTx,
  inputIsNotEqualTx,
  queryAddressUtxos,
  submitTransaction,
} from "../lib/index.js";
import * as csl from "@emurgo/cardano-serialization-lib-nodejs";
import * as fs from "node:fs/promises";

/*
 * The test suite!
 *
 * Notes:
 *  - We run the CLI command `demo-rts`, and use the configuration file it
 *    provides to run the show.
 *
 *  - Then, we run all the tests
 */
describe(`Typescript demo tests`, () => {
  /*
   * Start the runtime services by executing `demo-rts`
   */
  let demoRts: ChildProcess = undefined as unknown as ChildProcess;
  const demoRtsAbortController = new AbortController();

  // Start `demo-rts`
  // NOTE(jaredponn): we use the same cluster for all tests
  before(async () => {
    console.log(`Starting \`demo-rts\``);
    demoRts = spawn("demo-rts", [], {
      stdio: ["ignore", "pipe", "ignore"],
      signal: demoRtsAbortController.signal,
      killSignal: "SIGTERM",
    });

    // If demoRts fails, throw an error (unless we abort it manually)
    demoRts.on("error", (err) => {
      if (err.name === "AbortError") {
        return;
      } else {
        throw err;
      }
    });

    await new Promise((resolve) => {
      demoRts.stdout!.on(`data`, (data) => {
        resolve(data);
      });
    });

    await initRtsConfig("./demo-rtsconfig.json");
  });

  // Kill `demo-rts` when all the tests are done.
  after(async () => {
    await new Promise((resolve, reject) => {
      demoRts.on("close", (code, signal) => {
        if (signal === "SIGTERM") {
          return resolve(true);
        } else if (code === 0) {
          return resolve(true);
        } else {
          reject(code);
        }
      });
      demoRtsAbortController.abort();
    });
  });

  /*
   * Plutarch tests
   */
  it(`Plutarch`, async () => {
    const demoPlutarchConfig = await readDemoConfig(
      "data/demo-plutarch-config.json",
    );
    const plutarchEqValidatorBytes = demoPlutarchConfig.eqValidator;
    const plutarchEqValidator = csl.PlutusScript.new_v2(
      plutarchEqValidatorBytes,
    );

    const [exampleEqDatumA, exampleEqDatumB] = exampleData(demoPlutarchConfig);

    await eqValidatorTest(
      plutarchEqValidator,
      exampleEqDatumA,
      exampleEqDatumB,
    );

    return;
  });

  /*
   * PlutusTx tests
   */
  it(`PlutusTx`, async () => {
    // Grab the plutarch config (we still need this to generate the example data)
    const demoPlutarchConfig = await readDemoConfig(
      "data/demo-plutarch-config.json",
    );
    const [exampleEqDatumA, exampleEqDatumB] = exampleData(demoPlutarchConfig);

    // Grab the plutustx scripts that we actually want to test
    const demoPlutusTxConfig = await readDemoConfig(
      "data/demo-plutustx-config.json",
    );
    const plutustxEqValidatorBytes = demoPlutusTxConfig.eqValidator;
    const plutustxEqValidator = csl.PlutusScript.new_v2(
      plutustxEqValidatorBytes,
    );

    await eqValidatorTest(
      plutustxEqValidator,
      exampleEqDatumA,
      exampleEqDatumB,
    );

    return;
  });
});

/**
 * Reads the demo config from the provided filepath
 */
async function readDemoConfig(path: string): Promise<DemoConfig.Config> {
  const contents = await fs.readFile(path, { encoding: "utf8" });
  return LbrPrelude.Json[DemoConfig.Config].fromJson(
    PreludeJson.parseJson(contents),
  );
}

/**
 * Produces the example data that matches the CTL implementation (provided that
 * the argument `demoPlutarchConfig` really is the demoPlutarchConfig)
 */
function exampleData(
  demoPlutarchConfig: DemoConfig.Config,
): [EqDatum, EqDatum] {
  /*
   * Translate the `demoPlutarchConfig` validator address into a {@link PlaV1.Address}
   */
  const plutarchEqValidatorBytes = demoPlutarchConfig.eqValidator;

  const plutarchEqValidator = csl.PlutusScript.from_bytes(
    plutarchEqValidatorBytes,
  );
  const plutarchEqValidatorHash = plutarchEqValidator.hash();

  const plutarchEqValidatorStakeCredential = csl.StakeCredential
    .from_scripthash(plutarchEqValidatorHash);
  const plutarchEqValidatorEnterpriseAddress = csl.EnterpriseAddress.new(
    csl.NetworkId.mainnet().kind(),
    plutarchEqValidatorStakeCredential,
  );
  const plutarchEqValidatorAddress = plutarchEqValidatorEnterpriseAddress
    .to_address();

  const plutarchEqValidatorPlaAddress = cslPlutusDataToPlaPlutusData(
    csl.PlutusData.from_address(plutarchEqValidatorAddress),
  );

  /*
   * Building the example data
   */
  const exampleTokenName: PlaV1.TokenName = Prelude.fromJust(
    PlaV1.tokenNameFromBytes(
      Uint8Array.from(Buffer.from("example token name")),
    ),
  );
  const examplePlutusBytes = Uint8Array.from(Buffer.from("example bytes"));
  const exampleAssetClass: PlaV1.AssetClass = [
    PlaV1.adaSymbol,
    exampleTokenName,
  ];
  const exampleAddress: PlaV1.Address = LbrPlutusV1
    .IsPlutusData[LbrPlutusV1.Address].fromData(plutarchEqValidatorPlaAddress);

  const exampleEqDatumA: EqDatum = {
    rec: {
      bar: exampleAddress,
      baz: examplePlutusBytes,
      foo: exampleAssetClass,
    },
    sum: { name: "Baz", fields: examplePlutusBytes },
    prod: [exampleAssetClass, exampleAddress, examplePlutusBytes],
  };

  const exampleEqDatumB: EqDatum = {
    rec: {
      bar: exampleAddress,
      baz: examplePlutusBytes,
      foo: exampleAssetClass,
    },
    sum: { name: "Foo", fields: exampleAssetClass },
    prod: [exampleAssetClass, exampleAddress, examplePlutusBytes],
  };

  return [exampleEqDatumA, exampleEqDatumB];
}

export function plutusScriptToEnterpriseAddress(
  plutusScript: csl.PlutusScript,
): csl.EnterpriseAddress {
  const plutusScriptHash = plutusScript.hash();

  const stakeCredential = csl.StakeCredential.from_scripthash(plutusScriptHash);
  const enterpriseAddress = csl.EnterpriseAddress.new(
    csl.NetworkId.mainnet().kind(),
    stakeCredential,
  );
  return enterpriseAddress;
}

async function eqValidatorTest(
  eqValidator: csl.PlutusScript,
  exampleEqDatumA: EqDatum,
  exampleEqDatumB: EqDatum,
): Promise<void> {
  const eqValidatorAddress = plutusScriptToEnterpriseAddress(eqValidator)
    .to_address();

  /*
   * Finds the transaction input at the `eqValidatorAddress` which contains the
   * same  provided `datum`.
   */
  const findTxIn = async (datum: EqDatum): Promise<csl.TransactionInput> => {
    const utxos = await queryAddressUtxos(eqValidatorAddress);

    let datumUtxo: csl.TransactionInput | undefined = undefined;

    for (let i = 0; i < utxos.len(); ++i) {
      const utxo = utxos.get(i);

      const pd: csl.PlutusData | undefined = utxo.output().plutus_data();
      if (pd === undefined) {
        continue;
      }

      try {
        const eqDatum = LbrPlutusV1.IsPlutusData[EqDatum].fromData(
          cslPlutusDataToPlaPlutusData(pd),
        );
        if (LbrPrelude.Eq[EqDatum].eq(datum, eqDatum)) {
          datumUtxo = utxo.input();
          break;
        }
      } catch (e) {
        if (e instanceof PlaPd.IsPlutusDataError) {
          continue;
        } else {
          throw e;
        }
      }
    }

    if (datumUtxo === undefined) {
      throw new Error(`No UTxO found with eqValidator`);
    }

    return datumUtxo;
  };

  /*
   * Storing EqDatum at eqValidator
   */
  const createEqDatumATx = await createValueTx({
    eqValidatorAddress,
    eqDatum: exampleEqDatumA,
  });

  console.log("Storing EqDatum A @ EqV");
  const createEqDatumATxHash = await submitTransaction(createEqDatumATx);
  await awaitTransaction(createEqDatumATxHash);
  console.log(
    `Successfully stored EqDatum A @ EqV with ${createEqDatumATxHash.to_hex()}`,
  );

  /*
   * Checking if EqDatum A is the same as the one previously stored (it should be)
   */
  const eqDatumATxIn = await findTxIn(exampleEqDatumA);

  const eqDatumAIsEqualTx = await inputIsEqualTx(
    {
      eqValidator,
      eqValidatorTxIn: eqDatumATxIn,
      eqDatum: exampleEqDatumA,
    },
  );

  console.log(
    "Checking if EqDatum A is the same as the one previously stored (it should be)",
  );
  const eqDatumAIsEqualTxHash = await submitTransaction(eqDatumAIsEqualTx);
  await awaitTransaction(eqDatumAIsEqualTxHash);
  console.log(
    `Successfully checked that they are indeed the same in transaction ${eqDatumAIsEqualTxHash.to_hex()}`,
  );

  /*
   * Storing EqDatum at eqValidator
   */
  const createEqDatumBTx = await createValueTx({
    eqValidatorAddress,
    eqDatum: exampleEqDatumB,
  });

  console.log(`Storing EqDatum B @ EqVal`);
  const createEqDatumBTxHash = await submitTransaction(createEqDatumBTx);
  await awaitTransaction(createEqDatumBTxHash);
  console.log(
    `Successfully stored EqDatum B with ${createEqDatumBTxHash.to_hex()}`,
  );

  /*
   * Checking if EqDatum B is NOT the same as the one previously stored
   */
  const eqDatumBTxIn = await findTxIn(exampleEqDatumB);

  const eqDatumBIsEqualTx = await inputIsNotEqualTx(
    {
      eqValidator,
      eqValidatorTxIn: eqDatumBTxIn,
      eqDatum: exampleEqDatumA,
    },
  );

  console.log(
    `Checking if Eq Datum A is different to the one previously stored (it should be)`,
  );
  const eqDatumBIsEqualTxHash = await submitTransaction(eqDatumBIsEqualTx);
  await awaitTransaction(eqDatumBIsEqualTxHash);
  console.log(
    `Successfully check that they are indeed different in transaction ${eqDatumBIsEqualTxHash.to_hex()}`,
  );
}
