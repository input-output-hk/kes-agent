# JQ Expression for flake.lock

## Purpose

This document describes a `jq` expression to extract the name and `lastModified` field for every element of the `nodes` object in the `flake.lock` file.

## The JQ Expression

```bash
jq '.nodes | to_entries | map({name: .key, lastModified: .value.locked.lastModified})' flake.lock
```

## Explanation

The expression works as follows:

1. `.nodes` - Selects the `nodes` object from the flake.lock file
2. `to_entries` - Converts the object into an array of key-value pairs, where each entry has `.key` (the name) and `.value` (the node data)
3. `map({name: .key, lastModified: .value.locked.lastModified})` - Transforms each entry into an object with:
   - `name`: The node name (from the key)
   - `lastModified`: The timestamp from the `locked.lastModified` field

## Usage

### Direct Command

```bash
jq '.nodes | to_entries | map({name: .key, lastModified: .value.locked.lastModified})' flake.lock
```

### Using the Helper Script

A convenience script is provided:

```bash
./scripts/flake-lock-query.sh
```

Or specify a different flake.lock file:

```bash
./scripts/flake-lock-query.sh /path/to/other/flake.lock
```

## Example Output

```json
[
  {
    "name": "CHaP",
    "lastModified": 1770813616
  },
  {
    "name": "HTTP",
    "lastModified": 1451647621
  },
  {
    "name": "blst",
    "lastModified": 1739372843
  },
  ...
]
```

## Note

The `root` node will have `lastModified: null` as it does not have a `locked` field.
