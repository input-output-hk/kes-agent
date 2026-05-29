KES Agent Troubleshooting
=========================

This guide covers the situations operators most commonly hit when running the
KES agent, how to tell a real problem from a harmless warning, and where to
look. If you are setting up for the first time, also see the
[Migration guide](migration.markdown) and the
[Known-good-state checklist](guide.markdown#verifying-a-correct-setup-known-good-state).

First stop: `kes-agent-control info`
------------------------------------

Most questions are answered by querying the agent:

```sh
kes-agent-control info        # or: KES_AGENT_CONTROL_PATH=... kes-agent-control info
```

The output is divided into labelled blocks:

- **`--- Installed KES SignKey ---`** — the agent has an **active** key it is
  serving to nodes. This block reports the `VerKey`, `Valid from period`,
  `Current evolution: N / M`, and `OpCert number`. This is the block you want to
  see.
- **`--- Staged KES SignKey ---`** — a key has been generated but **not yet
  activated**; it is waiting for a matching OpCert (`install-key`). A node will
  not be served a staged key.
- **`--- Bootstrap Peers ---`** — status of connections to other agents (see
  below).

If `info` itself fails to connect, the agent is not running or you are pointing
at the wrong control socket — see *Socket and permission errors* below.

Reading the logs
----------------

When the agent runs as a systemd service it logs to the journal:

```sh
# follow the agent service live:
journalctl -u kes-agent -f

# everything the agent logged since the last boot:
journalctl -u kes-agent -b

# narrow to a time window:
journalctl -u kes-agent --since "10 min ago"
```

When run in the foreground (`kes-agent run ...`), it logs to stdout instead.

For the node side, watch `cardano-node`'s own logs for the leadership-check and
KES traces named in the
[Known-good-state checklist](guide.markdown#verifying-a-correct-setup-known-good-state).

Common situations
-----------------

### A bootstrap peer is stuck at `connecting...`

In `--- Bootstrap Peers ---`, each peer shows one of three statuses: `up`
(green), `connecting...` (yellow), or `down` (red). `connecting...` means the
agent has this peer in its configuration and is trying to reach it but has not
established a connection yet — and it will keep retrying indefinitely.

**This is usually harmless.** Bootstrap peers are an *optional* redundancy
mechanism for propagating a key between agents. If the agent already shows an
**Installed KES SignKey** and your node is forging, a peer stuck at
`connecting...` does **not** stop anything — it just means that particular peer
is not reachable right now.

The classic false alarm: you ran a foreground agent for testing, listed it as a
bootstrap peer, then moved to a systemd agent. The foreground agent no longer
exists, so it shows as permanently `connecting...`. Remove that address from the
agent's bootstrap configuration to clear the noise.

**When it *is* a problem:** if this agent has **no installed key** and is
*relying* on a bootstrap peer to receive one (e.g. a backup agent after a
reboot), then a peer that never leaves `connecting...` means the key is not
arriving. Check that the peer agent is running and actually holds a key, and
that the socket forwarding between them is up (see *SSH tunnel / multi-host*
below).

### The agent shows a Staged key but no Installed key

A staged key is not served to nodes. You still need to issue an OpCert for it
and install it:

```sh
kes-agent-control install-key --opcert-file opcert.cert
```

If `install-key` is rejected, the OpCert and the staged key do not match, or the
OpCert serial number is not greater than the last one used — re-issue the OpCert
from the air-gapped host's `opcert.counter` against the **current** staged
verification key (`export-staged-vkey`). See the [Migration guide](migration.markdown).

### The node is running but not producing blocks

Not producing blocks has many possible causes beyond KES. Isolate the layer:

- **KES** — does `kes-agent-control info` show an **Installed KES SignKey**, and
  is the node started with `--shelley-kes-agent-socket`? Do the node's KES log
  traces show the current KES period within the key's valid range?
- **OpCert** — is the node using the **same** OpCert that matches the installed
  key's verification key and serial number?
- **VRF** — is `--shelley-vrf-key` present and correct? (Unchanged by KES agent
  migration, but still required to forge.)
- **Topology / connectivity** — is the block producer connected to relays and
  fully synced (`cardano-cli query tip` → `syncProgress` `100.00`)?
- **Stake / schedule** — with low stake you may simply have no slots assigned in
  the current epoch. Confirm leadership checks are running before assuming a
  fault.

### Recovering after the agent restarted or the host rebooted

The KES sign key lives only in memory, so an agent restart or full reboot drops
it. What to do depends on your topology — this is covered in detail in
[Restart & Recovery](guide.markdown#restart--recovery). In short: a
**single-agent** setup needs the key re-installed (generate staged key → OpCert
→ `install-key`); a **backup-agent** setup re-fetches the key from its peer
automatically.

### Confirming the node reconnected after a restart

After restarting `cardano-node`, confirm it picked the key back up:

1. `kes-agent-control info` still shows an Installed KES SignKey.
2. The node logs show it connecting and KES traces appearing.
3. Leadership checks resume.

Run the full
[Known-good-state checklist](guide.markdown#verifying-a-correct-setup-known-good-state)
if in doubt.

### Socket and permission errors

If `kes-agent-control` cannot connect:

- Confirm the agent is running (`systemctl status kes-agent`, or check your
  foreground process).
- Confirm you are pointing at the correct **control** socket — via
  `-c <path>`, the `KES_AGENT_CONTROL_PATH` environment variable, or the default
  `/tmp/kes-agent-control.socket`. Note the control socket and the **service**
  socket (used by the node) are different sockets; do not mix them up.
- Confirm the user running the command can access the socket file. Access is
  governed by the socket's filesystem permissions — the user must be in the
  `kes-agent` group (see [Installing KES Agent](guide.markdown#installing-kes-agent)).

### "My change to the service file had no effect"

After editing the systemd unit or environment file, systemd keeps running the
old configuration until reloaded:

```sh
systemctl daemon-reload
systemctl restart kes-agent      # note: this drops the in-memory key — see Restart & Recovery
```

Confirm the running process actually reflects your change:

```sh
systemctl show kes-agent -p ExecStart -p EnvironmentFiles
ps -ef | grep kes-agent
```

Remember that **restarting the agent drops its in-memory key**; in a
single-agent setup you must re-install a key afterwards.

### SSH tunnel / multi-host issues

When agents (or the node and agent) run on different hosts, they communicate
over Unix domain sockets forwarded via SSH. If a connection that should be `up`
is `connecting...`/`down`:

- Confirm the `ssh`/`autossh` forwarding process is alive.
- Confirm stale local socket files are cleaned up — use
  `-o StreamLocalBindUnlink=yes` (see
  [Running on separate hosts](guide.markdown#running-cardano-node-kes-agent-control-andor-kes-agent-on-separate-hosts)).
- Confirm the forwarding maps the correct **service** sockets on both ends.

See also
--------

- [Migration guide](migration.markdown)
- [Restart & Recovery](guide.markdown#restart--recovery)
- [Known-good-state checklist](guide.markdown#verifying-a-correct-setup-known-good-state)
- [FAQ](faq.markdown)
