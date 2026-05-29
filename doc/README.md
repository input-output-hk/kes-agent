# KES Agent Documentation

Not sure where to start? Use this table.

| If you want to… | Read |
|-----------------|------|
| Understand what the KES Agent is and how it works, install it, and see recommended deployment setups | [User Guide](guide.markdown) |
| Move an existing block producer from an on-disk KES key to the agent | [Migration Guide](migration.markdown) |
| Diagnose a problem, read logs, or tell a real error from a harmless warning | [Troubleshooting](troubleshooting.markdown) |
| Understand the design rationale and security model (why keys aren't on disk, what survives a reboot, etc.) | [FAQ](faq.markdown) |

## Quick links within the guide

- [Glossary](guide.markdown#glossary) — staged vs. installed key, service vs. control socket, OpCert, host roles
- [Installation](guide.markdown#installation) — build prerequisites, tarball, Docker, building from source
- [Restart & Recovery](guide.markdown#restart--recovery) — what survives a node restart, agent restart, or full reboot
- [Verifying a correct setup](guide.markdown#verifying-a-correct-setup-known-good-state) — known-good-state checklist
- [Recommended Setups](guide.markdown#recommended-setups) — single-agent, backup-agent, and multi-agent topologies
