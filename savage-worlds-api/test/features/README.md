# API BDD Test Steps

This directory contains the Go step definitions for the BDD tests defined in the root `/features` directory.

## Setup

1. Install Godog:
```bash
make install-tools
```

2. Run BDD tests:
```bash
make test-bdd
```

## Writing Step Definitions

Create Go files in this directory to implement the step definitions for the features. Example:

```go
package features

import (
    "github.com/cucumber/godog"
)

func iAmAuthenticatedAsAGameMaster() error {
    // Implementation
    return nil
}

func InitializeScenario(ctx *godog.ScenarioContext) {
    ctx.Step(`^I am authenticated as a Game Master$`, iAmAuthenticatedAsAGameMaster)
}
```