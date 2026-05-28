Migrating An Existing Block Producer To The KES Agent
=====================================================

This guide walks through moving a running block-producing `cardano-node` from a
KES sign key **on disk** (`--shelley-kes-key kes.skey`) to receiving its key
from a KES agent over a socket (`--shelley-kes-agent-socket`).

It is written against the components every operator runs directly —
`cardano-node`, `cardano-cli`, and `kes-agent` / `kes-agent-control` — using
their real flags. If you manage your node through a wrapper or orchestration
script, use this as the authoritative reference and adapt your tooling
accordingly.

Before you start, make sure you understand the [Glossary](guide.markdown#glossary)
(staged vs. installed key, service vs. control socket) and the
[Restart & Recovery](guide.markdown#restart--recovery) behaviour — the KES sign
key lives **only in memory**, which changes how restarts and reboots behave.

Prerequisites
-------------

- `cardano-node` 11.x (it supports `--shelley-kes-agent-socket`).
- `kes-agent` and `kes-agent-control` installed on the node host (see
  [Installation](guide.markdown#installation)).
- Your cold **verification** key (`cold.vkey`) available on the node host — the
  agent needs it to verify OpCerts.
- An air-gapped signing host that holds the cold **sign** key (`cold.skey`) and
  the OpCert issue counter (`opcert.counter`). See
  [Setting Up An Air-gapped Signing Host](guide.markdown#setting-up-an-air-gapped-signing-host).
- Your existing `vrf.skey` (unchanged by this migration).

> **A note on the OpCert counter.** Migrating installs a **new** KES key, which
> means issuing a **new** OpCert with the next serial number. Always issue
> OpCerts from the air-gapped host's `opcert.counter` so the serial number keeps
> increasing; an out-of-order serial will be rejected.

Read this before you begin
--------------------------

A few ordering rules prevent the most common mistakes:

- **The KES sign key lives only in memory.** Restarting the agent before a key
  is installed leaves it empty.
- **Do the steps in order.** Generate the staged key → issue the OpCert →
  install the key → *then* point the node at the agent. If you switch the node
  to the agent socket before a key is installed, the node will have no key to
  forge with.
- **Do not delete your old `kes.skey` until the migration is verified.** Keep it
  until the node is confirmed forging through the agent (see the
  [Known-good-state checklist](guide.markdown#verifying-a-correct-setup-known-good-state)),
  so you can roll back if needed.

Step 1 — Start the KES agent
----------------------------

For the first run it is convenient to start the agent in the foreground so you
can watch its output. (We move it to a systemd service at the end.)

```sh
kes-agent run \
  -s service.socket \
  -c control.socket \
  --cold-verification-key cold.vkey \
  --genesis-file mainnet-shelley-genesis.json
```

Leave this running and open a second shell for the control commands. Point
`kes-agent-control` at the control socket either with `-c control.socket` on
each command, or by exporting it once:

```sh
export KES_AGENT_CONTROL_PATH=control.socket
```

Step 2 — Generate a staged KES key (control host)
-------------------------------------------------

First work out the current KES period (needed for the OpCert):

```sh
# slots per KES period, from the genesis file:
grep KESPeriod mainnet-shelley-genesis.json
# "slotsPerKESPeriod": 3600,

# current tip:
cardano-cli query tip --mainnet
# ... "slot": 26633911, ...

# current KES period = slot / slotsPerKESPeriod:
expr 26633911 / 3600
# 7398
```

Generate a staged key; this writes the matching verification key to a file:

```sh
kes-agent-control gen-staged-key \
  --kes-verification-key-file kes.vkey
```

Copy `kes.vkey` to your secure removable storage device and carry it to the
air-gapped signing host.

Step 3 — Issue an OpCert (air-gapped signing host)
--------------------------------------------------

```sh
cardano-cli node issue-op-cert \
  --kes-verification-key-file kes.vkey \
  --cold-signing-key-file cold.skey \
  --operational-certificate-issue-counter opcert.counter \
  --kes-period 7398 \
  --out-file opcert.cert
```

Copy `opcert.cert` back to the secure removable storage device and return it to
the control host. **Do not copy the cold sign key off this host.**

Step 4 — Install the key (control host)
---------------------------------------

```sh
kes-agent-control install-key --opcert-file opcert.cert
```

The agent verifies the OpCert against the staged key, promotes it to the
**installed (active)** key, and serves it to any connected nodes.

Step 5 — Verify the agent holds an active key
---------------------------------------------

```sh
kes-agent-control info
```

Confirm the output reports an **Installed KES SignKey** (not just a staged key).

Step 6 — Point cardano-node at the agent
-----------------------------------------

Change the node's start command: remove the on-disk key flag
(`--shelley-kes-key kes.skey`) and add `--shelley-kes-agent-socket`, pointing it
at the agent's **service** socket. Update `--shelley-operational-certificate` to
the **new** `opcert.cert` you just issued. The VRF key is unchanged.

```sh
cardano-node run \
  --config                          configuration.yaml \
  --topology                        topology.json \
  --database-path                   db \
  --socket-path                     node.sock \
  --shelley-kes-agent-socket        service.socket \
  --shelley-vrf-key                 vrf.skey \
  --shelley-operational-certificate opcert.cert \
  --port                            3001
```

Step 7 — Restart cardano-node
-----------------------------

Restart the node so it picks up the new command. On startup it connects to the
agent's service socket and is served the current evolution of the KES key.

Step 8 — Verify the migration
-----------------------------

Work through the
[Known-good-state checklist](guide.markdown#verifying-a-correct-setup-known-good-state).
When every item passes, the migration is complete. Only then is it safe to
securely remove the old `kes.skey` from the block producer.

Moving the agent into a systemd service
---------------------------------------

The foreground agent above is fine for the first migration, but for production
you want the agent managed by systemd so it starts on boot and is supervised.
See [Installing KES Agent](guide.markdown#installing-kes-agent) for the unit
files and `install.sh`.

Be aware of one subtlety that surprises operators: **the systemd agent is a
separate process from your foreground agent, with its own (empty) memory.**
Starting the service does not transfer the key from the foreground agent. So:

1. Stop the foreground agent (Ctrl-C in its shell).
2. Start the systemd service (`systemctl start kes-agent`). It comes up with no
   key.
3. Install a key into it exactly as in Steps 2–5, using the systemd agent's
   control socket path.
4. Restart `cardano-node` so it reconnects to the service socket the systemd
   agent listens on.

Do **not** configure the temporary foreground agent as a permanent bootstrap
peer of the systemd agent — it will not exist after the migration, and a dead
bootstrap peer left in the configuration will show as permanently
`connecting...` in `kes-agent-control info` (this is harmless but looks alarming;
see the [Troubleshooting guide](troubleshooting.markdown)).

If you want an agent that survives reboots without manual re-installation,
configure a **backup agent** on a second host instead (see
[Recommended Setups](guide.markdown#recommended-setups) and
[Restart & Recovery](guide.markdown#restart--recovery)).

See also
--------

- [Restart & Recovery](guide.markdown#restart--recovery)
- [Known-good-state checklist](guide.markdown#verifying-a-correct-setup-known-good-state)
- [Troubleshooting guide](troubleshooting.markdown)
- [FAQ](faq.markdown)
