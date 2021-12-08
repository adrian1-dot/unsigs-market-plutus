# unsigs market plutus

Updated [spacebudz](https://github.com/Berry-Pool/spacebudz) to newest plutus version, embedded in the [plutus-starter project](https://github.com/input-output-hk/plutus-starter). 

[`contract.hs`](https://github.com/Berry-Pool/spacebudz/blob/main/src/cardano/market/contract.hs) is split to `Onchain.hs` `Offchain.hs` `Types.hs`. `Utility.hs` is for the data needed to compile the contract.

## Run the Trace

1. Update cabal
```
cabal update
```

2. Build the project
```
cabal build
```

3. Use repl
```
cabal repl
```

4. Import the Trace module
```
import Trace
```

5. Run the Test
```
test
```
## The Plutus Application Backend (PAB) example

We have provided an example PAB application in `./pab`. With the PAB we can serve and interact
with contracts over a web API. You can read more about the PAB here: [PAB Architecture](https://github.com/input-output-hk/plutus-apps/blob/main/plutus-pab/ARCHITECTURE.adoc).


Here's an example of running and interacting with this contract via the API. For this it will help if you
have `jq` installed.

1. Build the PAB executable:

```
cabal build unsigs-market-pab
```

2. Run the PAB binary:

```
cabal exec -- unsigs-market-pab
````

This will then start up the server on port 9080. The devcontainer process will then automatically expose this port so that you can connect to it from any terminal (it doesn't have to be a terminal running in the devcontainer).

First, let's verify that the game is present in the server:

3. Check what contracts are present:

```
curl -s http://localhost:9080/api/contract/definitions | jq
```



Finally, also node that the PAB also exposes a websocket, which you can read about in
the general [PAB Architecture documentation](https://github.com/input-output-hk/plutus-apps/blob/main/plutus-pab/ARCHITECTURE.adoc).


## Support/Issues/Community

If you're looking for support, or would simply like to report a bug, feature
request, etc please do so over on the main [plutus
repository](https://github.com/input-output-hk/plutus).

For more interactive discussion, you can join the [IOG Technical Community
Discord](https://discord.gg/sSF5gmDBYg).

Thanks!