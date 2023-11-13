# LambdaBuffers Cardano Demo

This repository serves to demonstrate how to use the [LambdaBuffers](https://github.com/mlabs-haskell/lambda-buffers) toolkit for sharing types and their semantics between supported language environments Purescript, Haskell, Plutarch, PlutusTx and Cardano Transaction Library.

However, it can also serve as a scaffold for a typical Cardano dApp project that uses LambdaBuffers.

## Getting started

This repo relies on Nix and Direnv for seamless, reproducible and shareable builds and development environments. Please see the [LambdaBuffers: Getting started](https://mlabs-haskell.github.io/lambda-buffers/getting-started.html) chapter to help you set up these tools.

## Overview

The (mono)repo has several key directories and sub-projects, namely `api`, `validation` and `transactions`.

### API

The [api](api) directory contains the LambdaBuffers .lbf schemas where the Plutus and configuration types are specified for seamless sharing between project components.

Note the [api/build.nix](api/build.nix) file which is where the build recipes are specified for the .lbf schemas.

You can simply inspect the packages created by these build recipes with Nix:

```shell
$ nix build .#lbf-demo-plutus-api-haskell
$ find result/autogen/
result/autogen/
result/autogen/LambdaBuffers
result/autogen/LambdaBuffers/Demo
result/autogen/LambdaBuffers/Demo/Plutus.hs
```

You shouldn't ever need to do this, but it's always good to know how to peek under the hood when problems inevitably arise.

### Validation

The [validation](validation) directory contains the Plutus scripts that validate Plutus Protocol transactions (see [transactions](#transactions)).

This is traditionally called 'onchain' in other project templates, but I found it personally beneficial to rename these project constituents to make their meaning more clear and explicit.

This directory further contains two sub-projects, namely [validation/demo-plutarch](validation/demo-plutarch) and [validation/demo-plutustx](validation/demo-plutustx) that implement the same validation logic, but in Plutarch and PlutusTx respectively.

These projects implement the same Plutus protocol validation that consists of a single Equality Validator. This validator has a single 'spend' rule that checks whether the spent Eq Validator UTxO contains a [EqDatum](api/Demo/Plutus.lbf) equal or not equal to the [EqDatum](api/Demo/Plutus.lbf) specified in the [EqRedeemer](api/Demo/Plutus.lbf).

As is the case throughout this monorepo, the build.nix file contains the build specification parts of which can be inspected by simply using Nix:

```shell
# Build the Demo Json configuration that contains the Eq Validator compiled from the Plutarch specification.
$ nix build .#demo-plutarch-config
$ less result
# Build the Demo Json configuration that contains the Eq Validator compiled from the PlutusTx specification.
$ nix build .#demo-plutustx-config
$ less result
```

These configurations files are used by 'transactions' components that use the scripts contained within.

### Transactions

The [transactions](transactions) directory contains the transaction building components implemented various supported transaction building environments. Currently, only the Cardano Transaction Lib use is demonstrated in [transactions/demo-ctl](transactions/demo-ctl).

This is traditionally bundled in `offchain` directories in various project scaffolds, however, I found it useful to further delineate different parts of any given Cardano dApp project in several sub-parts. Along with 'transactions' one would need 'query' which is a component that serves to orchestrate fetching of the context necessary to build transactions.

There are 3 transactions in this Protocol specified in [transactions/ctl-demo/src/Demo/Transactions.purs](transactions/ctl-demo/src/Demo/Transactions.purs):

1. `create-value-tx` that stores an [EqDatum](api/Demo/Plutus.lbf) at the Eq validator,
2. `inputs-is-equal-tx` that spends a UTxO at the Eq validator and checks whether the [EqDatum](api/Demo/Plutus.lbf) spent is the same as the one provided in the redeemer,
3. `inputs-is-not-equal-tx` that spends a UTxO at the Eq validator and checks whether the [EqDatum](api/Demo/Plutus.lbf) spent is different as the one provided in the redeemer.

The testsuite spawns a new Cardano network and supporting services (uses Plutip, Ogmios and Kupo), builds the Protocol transactions in proper order and eventually signs and submits them.

As is the case throughout this monorepo, the build.nix file contains the build specification parts of which can be inspected by simply using Nix:

```shell
# Run the CTL check
$ nix -L --show-trace build .#checks.x86_64-linux.purescript:demo-ctl:check-nodejs
demo-ctl-check> CTL Demo tests » Plutarch
demo-ctl-check>   ✓︎ Store an example EqDatum at EqValidator and then check if (not)equal (8.48s)
demo-ctl-check> CTL Demo tests » PlutusTx
demo-ctl-check>   ✓︎ Store an example EqDatum at EqValidator and then check if (not)equal (4.44s)
demo-ctl-check> Summary
demo-ctl-check> 2/2 tests passed
```

The testsuite tries the same protocol flow with both configurations coming from Plutarch and PlutusTx scripts.
