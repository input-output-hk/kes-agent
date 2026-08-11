# Revision history for kes-agent-crypto

## 1.2.0.0

### Breaking

- Update to `cardano-crypto-class ^>=2.5`
- Require `cardano-binary >=1.9.1`, and use the `FixedSizeCodec` API from it in
  place of the raw serialisation functions deprecated by `cardano-crypto-class`

### Non-Breaking

- Widen `contra-tracer` bound to `>=0.1 && <0.3` (it had briefly been tightened
  to `^>=0.2.1`, which conflicts with dependents still on `^>=0.1`)

## 1.1.0.0 -- 2026-03-13

### Breaking

- Update to `cardano-crypto-class-2.3.1.0`

## 1.0.0.0 -- 2026-01-09

- First major release. See `kes-agent`'s `CHANGELOG.md` for details.
