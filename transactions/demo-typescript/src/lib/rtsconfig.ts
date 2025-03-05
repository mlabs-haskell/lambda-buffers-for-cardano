// This file includes code for creating the static set once readonly global
// configuration of the project

import * as csl from "@emurgo/cardano-serialization-lib-nodejs";
import { cborHexPrivateKey } from "./utils.js";

import * as fs from "node:fs/promises";

/**
 * The runtime system configuration of the application i.e., where the system
 * looks for
 *
 *  - the ogmios connection
 *
 *  - Secret key + its address
 */
export interface RtsConfig {
  /**
   * The ogmios configuration
   */
  ogmios: {
    host: string;
    port: number;
  };

  /**
   * `signingKey` used for paying fees
   */
  signingKey: csl.PrivateKey;

  /**
   * `signingKeyAddress` address used to pay fees and collect change
   */
  signingKeyAddress: csl.Address;
}

/**
 * The runtime system configuration of the application i.e., where the system
 * looks for
 *
 *  - the ogmios connection
 *
 *  - Secret key + its address
 */
export interface RawRtsConfig {
  /**
   * The ogmios configuration
   */
  ogmios: {
    host: string;
    port: string;
  };

  /**
   * `signingKey` used for paying fees
   */
  signingKeyCborHex: string;

  /**
   * `signingKeyAddress` address used to pay fees and collect change
   */
  signingKeyAddressBech32?: string;
}

/**
 * {@link readRtsConfig} reads the runtime configuration from the provided
 * file.
 *
 * Expects a JSON file of the form:
 * ```
 * {
 *  ogmios:
 *      { host: <host name e.g. "127.0.0.1">,
 *      , port: <port name e.g. 1337>
 *      },
 *  signingKeyCborHex: <string cbor hex of a signing key (e.g. the output from Cardano cli)>,
 *  signingKeyAddressBech32: <bech32 of the signingKeyCborHex's address>
 * }
 * ```
 * Note: `signingKeyAddressBech32` is optional.
 */
export async function readRtsConfig(path: string): Promise<RtsConfig> {
  const contents = await fs.readFile(path, { encoding: "utf8" });
  const json = JSON.parse(contents);
  return parseRtsConfig(json);
}

export function parseRtsConfig(json: RawRtsConfig): RtsConfig {
  const rtsConfig: RtsConfig = {
    ogmios: { host: undefined, port: undefined },
    signingKey: undefined,
    signingKeyAddress: undefined,
  } as unknown as RtsConfig;

  if (json?.ogmios?.host === undefined) {
    throw new Error(`demo: rts config missing ogmios.host`);
  }
  rtsConfig.ogmios.host = json.ogmios.host;

  if (json?.ogmios?.port === undefined) {
    throw new Error(`demo: rts config missing ogmios.port`);
  }
  rtsConfig.ogmios.port = parseInt(json.ogmios.port, 10);

  if (json.signingKeyCborHex === undefined) {
    throw new Error(`demo: rts config missing signingKeyCborHex`);
  }
  rtsConfig.signingKey = cborHexPrivateKey(json.signingKeyCborHex);

  if (json.signingKeyAddressBech32 === undefined) {
    console.error(
      `demo: rts config missing signingKeyAddressBech32 so computing an enterprise address from the provided signing key for the mainnet`,
    );
    const sk = rtsConfig.signingKey;
    const vk = sk.to_public();
    const vkHash = vk.hash();
    const credential = csl.Credential.from_keyhash(vkHash);
    const enterpriseAddress = csl.EnterpriseAddress.new(
      csl.NetworkId.testnet().kind(),
      credential,
    );
    console.error(
      `demo: computed \`SIGNING_KEY_ADDRESS_BECH32\` is \`${enterpriseAddress.to_address().to_bech32()}\``,
    );
    rtsConfig.signingKeyAddress = enterpriseAddress.to_address();
  } else {
    rtsConfig.signingKeyAddress = csl.Address.from_bech32(
      json.signingKeyAddressBech32,
    );
  }
  return rtsConfig;
}
