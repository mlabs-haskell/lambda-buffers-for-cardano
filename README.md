# LambdaBuffers Cardano Demo

This repository serves to demonstrate how to use the
[LambdaBuffers](https://github.com/mlabs-haskell/lambda-buffers) toolkit for
sharing types and their semantics between supported language environments
Plutarch, PlutusTx, Purescript (with Cardano Transaction Library), Haskell,
Rust, and TypeScript.

However, it can also serve as a scaffold for a typical Cardano dApp project
that uses LambdaBuffers.

## Getting started

This repo relies on Nix and Direnv for seamless, reproducible and shareable
builds and development environments. Please see the
[LambdaBuffers: Getting started](https://mlabs-haskell.github.io/lambda-buffers/getting-started.html)
chapter to help you set up these tools.

## Overview

The (mono)repo has several key directories and sub-projects, namely `api`,
`validation` and `transactions`.

### API

The [api](api) directory contains the LambdaBuffers .lbf schemas where the
Plutus and configuration types are specified for seamless sharing between
project components.

Note the [api/build.nix](api/build.nix) file which is where the build recipes
are specified for the .lbf schemas.

You can simply inspect the packages created by these build recipes with Nix.
For example, to inspect the Haskell package created by the aforementioned
build recipe, you can execute the following.

```shell
$ nix build .#lbf-demo-plutus-api-haskell
$ find result/autogen/
result/autogen/
result/autogen/LambdaBuffers
result/autogen/LambdaBuffers/Demo
result/autogen/LambdaBuffers/Demo/Plutus.hs
```

You shouldn't ever need to do this, but it's always good to know how to peek
under the hood when problems inevitably arise.

### Validation

The [validation](validation) directory contains the Plutus scripts that validate
Plutus Protocol transactions (see [transactions](#transactions)).

This is traditionally called 'onchain' in other project templates, but I found it
personally beneficial to rename these project constituents to make their meaning
more clear and explicit.

This directory further contains two sub-projects, namely
[validation/demo-plutarch](validation/demo-plutarch) and
[validation/demo-plutustx](validation/demo-plutustx) that implement the same
validation logic, but in Plutarch and PlutusTx respectively.

These projects implement the same Plutus protocol validation that consists of a
single Equality Validator. This validator has a single 'spend' rule that checks
whether the spent Eq Validator UTxO contains a [EqDatum](api/Demo/Plutus.lbf)
equal or not equal to the [EqDatum](api/Demo/Plutus.lbf) specified in the
[EqRedeemer](api/Demo/Plutus.lbf).

As is the case throughout this monorepo, the build.nix file contains the build
specification parts of which can be inspected by simply using Nix:

```shell
# Build the Demo Json configuration that contains the Eq Validator compiled from
# the Plutarch specification.
$ nix build .#demo-plutarch-config
$ less result
# Build the Demo Json configuration that contains the Eq Validator compiled from
# the PlutusTx specification.
$ nix build .#demo-plutustx-config
$ less result
```

These configurations files are used by 'transactions' components that use the
scripts contained within.

### Transactions

The [transactions](transactions) directory contains the transaction building
components implemented various supported transaction building environments.
Currently, we demonstrate transaction building with the following systems:

- PureScript with Cardano Transaction Lib in
  [transactions/demo-ctl](transactions/demo-ctl).

- JavaScript (TypeScript) with Cardano Serialization Lib in
  [transactions/demo-typescript](transactions/demo-typescript).

- Rust with [tx-village](https://github.com/mlabs-haskell/tx-village) in
  [transactions/demo-rust](transactions/demo-rust).

- Haskell using Plutus Ledger API types to build data structures suitable for
  tx-village in [transactions/demo-haskell](transactions/demo-haskell) which
  uses a Rust CLI to tx-village in
  [transactions/demo-tx-village](transactions/demo-tx-village).

The transaction building is traditionally bundled in `offchain` directories of
various project scaffolds, however, I found it useful to further delineate
different parts of any given Cardano dApp project in several sub-parts. Along
with 'transactions' one would need 'query' which is a component that serves to
orchestrate fetching of the context necessary to build transactions. However,
owing to the simplicity of this Protocol, the query component is simply included
within the transaction building projects.

There are 3 transactions in this Protocol as follows.

1. `create-value-tx` that stores an [EqDatum](api/Demo/Plutus.lbf) at the Eq
   validator,
2. `inputs-is-equal-tx` that spends a UTxO at the Eq validator and checks whether
   the [EqDatum](api/Demo/Plutus.lbf) spent is the same as the one provided in the
   redeemer,
3. `inputs-is-not-equal-tx` that spends a UTxO at the Eq validator and checks
   whether the [EqDatum](api/Demo/Plutus.lbf) spent is different as the one
   provided in the redeemer.

Each of the projects has a testsuite which spawns a new Cardano network (using
cardano-devnet) and supporting services (Ogmios, and Kupo for PureScript with
CTL), builds the Protocol transactions in proper order and eventually signs and
submits them.

The testsuite tries the same protocol flow with both configurations coming from
Plutarch and PlutusTx scripts where the testsuite executes the following:

1. Build and submit a `create-value-tx` transaction to store a EqDatum at an Eq
   validator.

2. Build and submit a `inputs-is-equal-tx` transaction to check the EqDatum is the
   same as the one provided in the redeemer

3. Build and submit another `create-value-tx` transaction to store a EqDatum at an
   Eq validator.

4. Build and submit a `inputs-is-not-equal-tx` transaction to check the EqDatum is
   not the same as the one provided in the redeemer

As is the case throughout this monorepo, the build.nix file contains the build
specification parts of which can be inspected by simply using Nix.

#### Testsuites

Testsuites use process-compose to manage the runtime dependencies, such as
cardano-node, ogmios, etc. required for the tests. These can all be executed
either by a nix command, like

```shell
$ nix build .#checks.x86_64-linux.demo-rust-checks -L
...
```

or by entering the direnv shell of the given project, and executing the process
compose command exposed by Nix:

```shell
$ cd transactions/demo-rust
$ pc-demo-rust-tests --keep-project
...
```

##### PureScript

The transactions in this Protocol are implemented in
[transactions/demo-ctl/src/Demo/Transactions.purs](transactions/demo-ctl/src/Demo/Transactions.purs).

The testsuite can be executed with the following commands.

```shell
# Run the CTL check
$ nix build .#checks.x86_64-linux.demo-ctl-checks -L
...
```

or

```shell
$ cd transactions/demo-ctl
$ pc-demo-ctl-tests --keep-project
...
```

##### Rust

The transactions in this Protocol are implemented in [transactions/demo-rust/src/lib.rs](transactions/demo-rust/src/lib.rs).

The testsuite can be executed with the following commands.

```shell
# Run the Rust check
$ nix build .#checks.x86_64-linux.demo-rust-checks -L
...
```

or

```shell
$ cd transactions/demo-rust
$ pc-demo-rust-tests --keep-project
...
```

##### JavaScript (TypeScript)

The transactions in this Protocol are implemented in [transactions/demo-typescript/src/lib/index.ts](transactions/demo-typescript/src/lib/index.ts).

The testsuite can be executed with the following commands.

```shell
# Run the TypeScript check
$ nix build .#checks.x86_64-linux.demo-typescript-checks -L
...
```

or

```shell
$ cd transactions/demo-typescript
$ pc-demo-typescript-tests --keep-project
...
```

##### Haskell and Rust's tx-village library

The Haskell implementation of the transactions for the Protocol differs from the
previous projects in the sense that all of the previous projects built and
submitted transactions themselves.
The Haskell project instead only builds an intermediate data structure, a
`TxInfo`, which contains sufficient information to build the transactions of the
project.
The `TxInfo` may then be encoded to JSON using LambdaBuffers and is provided to
the `demo-tx-village` Rust CLI which builds a transaction from the `TxInfo` and
submits the transaction.

The Haskell implementation for building `TxInfo`s of the transactions for the
Protocol can be found in [transactions/demo-haskell/src/Process.hs](transactions/demo-haskell/src/Process.hs).

The Rust implementation for building transactions given a `TxInfo` from the
Haskell project can be found in [transactions/demo-tx-village/src/build_and_submit.rs](transactions/demo-tx-village/src/build_and_submit.rs).

The testsuite which runs the integration test of piping Haskell's `TxInfo`s to
Rust, and building and submitting the transactions to the blockchain can be
executed with the following commands.

```shell
# Run the Rust check
$ nix build .#checks.x86_64-linux.demo-tx-village-checks -L
...
```

or

```shell
$ cd transactions/demo-tx-village
$ pc-demo-tx-village-tests --keep-project
...
```
