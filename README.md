# analytics-core

Core analytics and inference engine implemented in Haskell.

## Overview

`analytics-core` provides the foundational data structures, reasoning primitives, graph operations, evidence management, rule evaluation, and query execution capabilities used by the Analytics platform.

## Features

* Fact and evidence management
* Rule-based inference
* Knowledge graph support
* Query execution engine
* Contradiction detection
* Event modeling
* Extensible plugin architecture

## Project Structure

```text
src/
├── Analytics/
│   └── Core/
│       ├── Contradiction.hs
│       ├── Evidence.hs
│       ├── Fact.hs
│       ├── Graph.hs
│       ├── Inference.hs
│       ├── KnowledgeBase.hs
│       ├── Plugin.hs
│       ├── Query.hs
│       ├── Rule.hs
│       ├── Runtime.hs
│       ├── Storage.hs
│       ├── Types.hs
│       └── Event/
```

## Requirements

* GHC 9.6+
* Stack

## Build

```bash
stack build
```

## Run Tests

```bash
stack test
```

## Development

```bash
stack ghci
```

## License

Proprietary © Bombay Boyz

