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
    * Install `hlint`: `stack install hlint`
    * Install `stylish-haskell`: `stack install stylish-haskell`
1. Run `hpack` if changes are made to `package.yaml` or new modules are added.

### Continuous Compilation with GHCID

The following examples require `ghcid`. Install it via `stack install ghcid`.

Run `ghcid` while developing to compile and reload on code changes. Here are a few options

* Continuously compile application

    ```bash
    ghcid --command="stack ghci wav-metadata-service"
    ```

## WAV Metadata

This service responds with the WAV metadata of the specified files.
The `/upload` endpoint supports multiple files, and therefore will respond with an array of parsed WAV files.

In the case where the file can't be successfully parsed, the endpoint will still return a 200 response with a generic error message.

The `data` of the `data` chunk is omitted as it contains no useful metadata; it represents the audible contents of the file.

### Parsing Valid WAV Files

1. Start the application: `stack run`
2. Submit a WAV file: `curl -F "file=@./assets/wav/about_time.wav" http://localhost:8080/upload`
3. Verify that the endpoint returns the decoded WAV metadata

    ```json
    [
        {
            "about_time.wav": {
                "fmt": {
                    "blockAlign": 1,
                    "audioFormat": 1,
                    "size": 16,
                    "numChannels": 1,
                    "sampleRate": 11025,
                    "id": "fmt",
                    "bitsPerSample": 8,
                    "byteRate": 11025
                },
                "data": {
                    "size": 28800,
                    "data": "<omitted>",
                    "id": 25697
                },
                "riff": {
                    "size": 113,
                    "format": "WAVE",
                    "id": "RIFF"
                }
            }
        }
    ]
    ```

### Parsing Invalid WAV Files

1. Start the application: `stack run`
2. Create or load some invalid file: `touch /path/to/invalid/file`
3. Submit a WAV file: `curl -F "file=@/path/to/invalid/file" http://localhost:8080/upload`
4. Verify that the endpoint responds with an error message

    ```json
    [
        {
            "/path/to/invalid/file": "Not a valid WAV file"
        }
    ]
    ```

5. Look in the server log for the reason for the failure
