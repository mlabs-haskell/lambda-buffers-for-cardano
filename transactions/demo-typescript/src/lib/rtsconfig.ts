import {
  createInteractionContext,
  InteractionContext,
} from "@cardano-ogmios/client";
import * as csl from "@emurgo/cardano-serialization-lib-nodejs";
import { cborHexPrivateKey } from "./utils.js";

export interface RtsConfig {
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

function getOgmiosHost() {
  const ogmiosHost: string | undefined = process.env["OGMIOS_HOST"];
  return ogmiosHost !== undefined ? ogmiosHost : "127.0.0.1";
}

function getOgmiosPort() {
  const ogmiosPort: string | undefined = process.env["OGMIOS_PORT"];
  return ogmiosPort !== undefined ? parseInt(ogmiosPort, 10) : 1337;
}

function getSigningKey() {
  const signingKeyCborHex: string | undefined =
    process.env["SIGNING_KEY_CBOR_HEX"];
  if (signingKeyCborHex === undefined) {
    throw new Error(
      `Environment variable \`SIGNING_KEY_CBOR_HEX\` is undefined. Please provide the cbor hex of a signing key (probably from \`cardano-cli\`)`,
    );
  }

  return cborHexPrivateKey(signingKeyCborHex);
}

function getSigningKeyAddress() {
  const addrBech32: string | undefined =
    process.env["SIGNING_KEY_ADDRESS_BECH32"];

  if (addrBech32 === undefined) {
    console.error(
      `demo: environment variable \`SIGNING_KEY_ADDRESS_BECH32\` was not provided, so computing an enterprise address from the provided signing key for the mainnet`,
    );
    const sk = getSigningKey();
    const vk = sk.to_public();
    const vkHash = vk.hash();
    const stakeCredential = csl.StakeCredential.from_keyhash(vkHash);
    const enterpriseAddress = csl.EnterpriseAddress.new(
      csl.NetworkId.mainnet().kind(),
      stakeCredential,
    );
    console.error(
      `demo: computed \`SIGNING_KEY_ADDRESS_BECH32\` is \`${enterpriseAddress.to_address().to_bech32()}\``,
    );
    return enterpriseAddress.to_address();
  }

  return csl.Address.from_bech32(addrBech32);
}

/**
 * Global configuration of the application
 */
export const rtsConfig: RtsConfig = {
  ogmios: {
    host: getOgmiosHost(),
    port: getOgmiosPort(),
  },
  signingKey: getSigningKey(),
  signingKeyAddress: getSigningKeyAddress(),
};

/** */
export async function ogmiosCreateContext(): Promise<InteractionContext> {
  const ogmios = rtsConfig.ogmios;
  const context = await createInteractionContext(
    (err) => {
      throw err;
    },
    () => {
      return;
    },
    { connection: ogmios },
  );
  return context;
}
