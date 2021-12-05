{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# AllowAmbiguousTypes #-}

module Trace
    ( test
    ,  idd 
    , params
    , assetTn0
    ) where


import Plutus.Trace.Emulator as Emulator
    ( activateContractWallet, EmulatorTrace, waitNSlots, runEmulatorTraceIO', callEndpoint, EmulatorConfig(..) )
import           Control.Monad    (void)
import           PlutusTx.Prelude as Plutus ( ($), (<>), Either(..),  (++), Integer)
import           PlutusTx.Builtins.Class
import           Ledger.Value     as Value (singleton)
import qualified Data.Map         as Map
import qualified Ledger.Ada       as Ada
import           Plutus.V1.Ledger.Value
import           Prelude      (IO, String, show, putStrLn)
import           Data.Default (def)
import           Plutus.Contracts.ErrorHandling
import           Plutus.Contract
import           Plutus.Contract.Trace
import           PlutusTx.Builtins.Internal (BuiltinByteString (..), BuiltinString, encodeUtf8)


import Offchain 
import Types
import Utility (wallet, prefixToken, prefixBidToken, assetSymbol1, assetSymbol2)

assetTn0 :: TokenName
assetTn0  = TokenName { unTokenName = "SpaceBudBid0" }

assetTn1 :: TokenName
assetTn1 = TokenName { unTokenName = "SpaceBudBid1" }

assetTn2 :: TokenName 
assetTn2 = TokenName { unTokenName = "SpaceBudBid2" }


idd :: BuiltinByteString
idd = encodeUtf8 $ stringToBuiltinString "1"

params :: TradeParams
params = TradeParams { id = idd, amount = 1 :: Integer}

test :: IO ()
test = do
    let dist = Map.fromList [ (wallet 1, Ada.lovelaceValueOf 100_000_000
                                      <> Value.singleton assetSymbol2 assetTn0 1
				      <> Value.singleton assetSymbol2 assetTn1 1
				      <> Value.singleton assetSymbol2 assetTn2 1)
                            , (wallet 2, Ada.lovelaceValueOf 100_000_000)
                            , (wallet 3, Ada.lovelaceValueOf 100_000_000)
                            , (wallet 4, Ada.lovelaceValueOf 100_000_000)
                            , (wallet 5, Ada.lovelaceValueOf 100_000_000)
                            , (wallet 6, Ada.lovelaceValueOf 100_000_000)
                            ]
        emCfg = EmulatorConfig (Left dist) def def
    runEmulatorTraceIO' def emCfg $ do
        h1 <- activateContractWallet (wallet 1) Trace.contract
        h2 <- activateContractWallet (wallet 2) Trace.contract
        h3 <- activateContractWallet (wallet 3) Trace.contract
        h4 <- activateContractWallet (wallet 4) Trace.contract
        h5 <- activateContractWallet (wallet 5) Trace.contract
        h6 <- activateContractWallet (wallet 6) Trace.contract
	void $ Emulator.waitNSlots 1	    	    
	callEndpoint @"init" h1 ()
	void $ Emulator.waitNSlots 1
        --callEndpoint @"bid" h2 params 
        void $ Emulator.waitNSlots 1


contract :: Contract () TradeSchema ContractError ()
contract = endpoints
