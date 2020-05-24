# wav-metadata-service

## Overview

The goal of this service is to provide an endpoint that uses [haskell-wav](https://github.com/michaelheyman/haskell-wav) to respond to an HTTP request with the metadata of the submitted WAV file.

## How to Develop

### Running the Application

1. Install dependencies: `stack install`
2. Start the application: `stack run`
3. Verify that <http://localhost:8080/users> returns data and a 200.

### Before Committing Changes

1. Ensure that `pre-commit` and its configuration are installed
    * Install `pre-commit`: `brew install pre-commit` or `pip install pre-commit`
    * Install the commit hooks: `pre-commit install`
1. Run `hpack` if changes are made to `package.yaml` or new modules are added.
