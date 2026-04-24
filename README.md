# KES Agent

A sidecar daemon for `cardano-node` that holds KES signing keys in mlocked memory, replacing on-disk key files.

[![Haskell CI](https://github.com/input-output-hk/kes-agent/actions/workflows/haskell.yml/badge.svg)](https://github.com/input-output-hk/kes-agent/actions/workflows/haskell.yml)
[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE)

## Overview

KES (Key Evolving Signature) keys must never be stored on disk: once a key evolution is deleted, an attacker who later compromises the host cannot reconstruct past signatures. KES Agent is a standalone process that keeps the current KES sign key in mlocked RAM, evolves it autonomously every KES period (~36 hours), and hands it to `cardano-node` over a local Unix socket. Because the key lives only in memory, it survives node restarts without ever touching persistent storage.

For production installation, system hardening, multi-agent setups, and key rotation procedures, see the [User Guide](doc/guide.markdown).

## Prerequisites

- **Platform:** Linux only. Windows builds compile but are not supported and will not work correctly.
- **cardano-node:** 10.7.1 or later (the first version with KES Agent socket support).
- **Haskell toolchain:** GHC and Cabal (install via [GHCup](https://www.haskell.org/ghcup/)).
- **System libraries:** `libsodium`, `secp256k1`, and `libblst`.
  - `libblst` requires manual installation of headers and `libblst.a` into system-wide locations and a `pkgconf` entry. See the [User Guide](doc/guide.markdown#build-prerequisites) for details.

## Quick Start

Pre-built installer tarballs are available on the [Releases](https://github.com/input-output-hk/kes-agent/releases) page. To build from source instead, follow the steps below.

### Build & Install

```sh
git clone https://github.com/input-output-hk/kes-agent/ ./kes-agent
cd kes-agent
cabal update
cabal install exe:kes-agent exe:kes-agent-control
```

### Run

```sh
kes-agent run \
    --service-address       /path/to/service.socket \
    --control-address       /path/to/control.socket \
    --cold-verification-key /path/to/cold.vkey \
    --genesis-file          /path/to/shelley-genesis.json
```

### Verify

```sh
kes-agent-control --control-address /path/to/control.socket info
```

## Development

### Running Tests

```sh
cabal test all
```

## License & Copyright

Copyright INTERSECT 2024-2025.

Licensed under the Apache License, Version 2.0 (the "License"); see the
enclosed NOTICE and LICENSE files.
