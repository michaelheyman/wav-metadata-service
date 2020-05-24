# wav-metadata-service

## Overview

The goal of this service is to provide an endpoint that uses [haskell-wav](https://github.com/michaelheyman/haskell-wav) to respond to an HTTP request with the metadata of the submitted WAV file.

## How to Develop

### Running the Application

1. Install dependencies: `stack install`
2. Start the application: `stack run`
3. Create a file and execute: `curl -F "data=@<FILE>" http://localhost:8080/upload`
4. Verify that the endpoint returns `The form was submitted with 0 textual inputs and 1 files.`

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
