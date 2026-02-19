#!/usr/bin/env bash
# Script to extract name and lastModified from flake.lock nodes
# Usage: ./scripts/flake-lock-query.sh [flake.lock path]

set -euo pipefail

FLAKE_LOCK="${1:-flake.lock}"

if [ ! -f "$FLAKE_LOCK" ]; then
    echo "Error: flake.lock file not found at: $FLAKE_LOCK" >&2
    exit 1
fi

# JQ expression to extract name and lastModified for every element of nodes
jq '.nodes | to_entries | map({name: .key, lastModified: .value.locked.lastModified})' "$FLAKE_LOCK"
