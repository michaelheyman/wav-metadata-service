# wav-metadata-service

## Overview

This service provides an endpoint that uses [haskell-wav](https://github.com/michaelheyman/haskell-wav) to respond to the request with the metadata of the submitted WAV file.

## How to Develop

### Running the Application

1. Install dependencies: `stack install`
2. Start the application: `stack run`
3. Submit a valid WAV file: `curl -F "data=@path/to/wav" http://localhost:8080/upload`
4. Verify that the endpoint returns the decoded WAV metadata

### Before Committing Changes

1. Ensure that `pre-commit` and its configuration are installed
    * Install `pre-commit`: `brew install pre-commit` or `pip install pre-commit`
    * Install the commit hooks: `pre-commit install`
1. Run `hpack` if changes are made to `package.yaml` or new modules are added.

### Continuous Compilation with GHCID

The following examples require `ghcid`. Install it via `stack install ghcid`.

Run `ghcid` while developing to compile and reload on code changes. Here are a few options

* Continuously compile application

    ```bash
    ghcid --command="stack ghci wav-metadata-service"
    ```
