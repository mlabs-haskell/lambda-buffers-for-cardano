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
import { readRtsConfig, RtsConfig } from "../lib/rtsconfig.js";
import { Demo } from "../lib/index.js";
import * as csl from "@emurgo/cardano-serialization-lib-nodejs";
import * as fs from "node:fs/promises";
import process from "node:process";

/*
 * The test suite!
 *
 * Notes:
 *  - If one sets the environment variable `TEST_RTS_CONFIG`, the file
 *    `$TEST_RTS_CONFIG` will be used in {@link initRtsConfig}.
 *
 *  - Otherwise, this will run the CLI command `demo-rts` (which starts all the
 *    runtime services), and use the configuration file `demo-rts` provides to
 *    run the show.
 *
 *  - In either case, we run all the tests afterwards.
 */
describe(`Typescript demo tests`, async () => {
  /*
   * Start the runtime services by executing `demo-rts` (or use the existence
   * runtime services if `TEST_RTS_CONFIG` is set)
   */
  let demoRts: ChildProcess = undefined as unknown as ChildProcess;
  let rtsConfig: RtsConfig = undefined as unknown as RtsConfig;
  const testRtsConfigFilePath = process.env[`TEST_RTS_CONFIG`];

  function killDemoRts() {
    if (demoRts.killed === false) {
      process.kill(-demoRts.pid!, "SIGTERM");
    }
  }

  // Start `demo-rts`
  // NOTE(jaredponn): we use the same cluster for all tests
  before(async () => {
    if (testRtsConfigFilePath !== undefined) {
      rtsConfig = await readRtsConfig(testRtsConfigFilePath);
    } else {
      demoRts = spawn("demo-rts", [], {
        stdio: ["ignore", "pipe", "ignore"],
        detached: true,
        env: Object.assign({ "ADA": "10" }, process.env),
      });

      await new Promise((resolve) => {
        demoRts.stdout!.on(`data`, (data) => {
          resolve(data);
        });
      });
      rtsConfig = await readRtsConfig("./demo-rtsconfig.json");
    }
  });

  after(async () => {
    // Kill `demo-rts` when all the tests are done.
    if (testRtsConfigFilePath === undefined) {
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
        killDemoRts();
      });
    }
  });

  /*
   * Plutarch tests
   */
  await it(`Plutarch`, async () => {
    const demoPlutarchConfig = await readDemoConfig(
      "data/demo-plutarch-config.json",
    );
    const plutarchEqValidatorBytes = demoPlutarchConfig.eqValidator;
    const plutarchEqValidator = csl.PlutusScript.new_v2(
      plutarchEqValidatorBytes,
    );

    const [exampleEqDatumA, exampleEqDatumB] = exampleData(demoPlutarchConfig);

    const demo = await Demo.new(rtsConfig);

    await eqValidatorTest(
      demo,
      plutarchEqValidator,
      exampleEqDatumA,
      exampleEqDatumB,
    );
  });

  /*
   * PlutusTx tests
   */
  await it(`PlutusTx`, async () => {
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

    const demo = await Demo.new(rtsConfig);
    await eqValidatorTest(
      demo,
      plutustxEqValidator,
      exampleEqDatumA,
      exampleEqDatumB,
    );
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
 * the argument `demoPlutarchConfig` really is the demo Plutarch config)
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
  demo: Demo,
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
    const utxos = await demo.query.queryAddressUtxos(eqValidatorAddress);

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
  const createEqDatumATx = await demo.createValueTx({
    eqValidatorAddress,
    eqDatum: exampleEqDatumA,
  });

  console.log("Storing EqDatum A @ EqV");
  const createEqDatumATxHash = await demo.query.submitTx(createEqDatumATx);
  await demo.query.awaitTx(createEqDatumATxHash);
  console.log(
    `Successfully stored EqDatum A @ EqV with ${createEqDatumATxHash.to_hex()}`,
  );

  /*
   * Checking if EqDatum A is the same as the one previously stored (it should be)
   */
  const eqDatumATxIn = await findTxIn(exampleEqDatumA);

  const eqDatumAIsEqualTx = await demo.inputIsEqualTx(
    {
      eqValidator,
      eqValidatorTxIn: eqDatumATxIn,
      eqDatum: exampleEqDatumA,
    },
  );

  console.log(
    "Checking if EqDatum A is the same as the one previously stored (it should be)",
  );
  const eqDatumAIsEqualTxHash = await demo.query.submitTx(eqDatumAIsEqualTx);
  await demo.query.awaitTx(eqDatumAIsEqualTxHash);
  console.log(
    `Successfully checked that they are indeed the same in transaction ${eqDatumAIsEqualTxHash.to_hex()}`,
  );

  /*
   * Storing EqDatum at eqValidator
   */
  const createEqDatumBTx = await demo.createValueTx({
    eqValidatorAddress,
    eqDatum: exampleEqDatumB,
  });

  console.log(`Storing EqDatum B @ EqVal`);
  const createEqDatumBTxHash = await demo.query.submitTx(createEqDatumBTx);
  await demo.query.awaitTx(createEqDatumBTxHash);
  console.log(
    `Successfully stored EqDatum B with ${createEqDatumBTxHash.to_hex()}`,
  );

  /*
   * Checking if EqDatum B is NOT the same as the one previously stored
   */
  const eqDatumBTxIn = await findTxIn(exampleEqDatumB);

  const eqDatumBIsEqualTx = await demo.inputIsNotEqualTx(
    {
      eqValidator,
      eqValidatorTxIn: eqDatumBTxIn,
      eqDatum: exampleEqDatumA,
    },
  );

  console.log(
    `Checking if Eq Datum A is different to the one previously stored (it should be)`,
  );
  const eqDatumBIsEqualTxHash = await demo.query.submitTx(eqDatumBIsEqualTx);
  await demo.query.awaitTx(eqDatumBIsEqualTxHash);
  console.log(
    `Successfully check that they are indeed different in transaction ${eqDatumBIsEqualTxHash.to_hex()}`,
  );
}
