{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# AllowAmbiguousTypes #-}

module Trace
    ( test
    , assetTn0
    ) where


import Plutus.Trace.Emulator as Emulator
    ( activateContractWallet, EmulatorTrace, waitNSlots, runEmulatorTraceIO', callEndpoint, EmulatorConfig(..) )
import           Control.Monad    (void)
import           PlutusTx.Prelude as Plutus ( ($), (<>), Either(..),  (++), Integer, (+), (*))
import           PlutusTx.Builtins.Class
import           Ledger.Value     as Value (singleton)
import qualified Data.Map         as Map
import qualified Ledger.Ada       as Ada
import           Plutus.V1.Ledger.Value
import           Prelude      (IO, String, show, putStrLn, print)
import           Data.Default (def)
import           Plutus.Contracts.ErrorHandling
import           Plutus.Contract
import           Plutus.Contract.Trace
import           PlutusTx.Builtins.Internal (BuiltinByteString (..), BuiltinString, encodeUtf8)
import           Data.Text (Text)

import Offchain 
import Types
import Utility (wallet, prefixToken, prefixBidToken, assetSymbol1, assetSymbol2)

assetTn0 :: TokenName
assetTn0  = TokenName { unTokenName = "UnsigsBid0" }

assetTn1 :: TokenName
assetTn1 = TokenName { unTokenName = "UnsigsBid1" }

assetTn2 :: TokenName 
assetTn2 = TokenName { unTokenName = "UnsigsBid2" }

params0 :: Integer -> TradeParams
params0 i = TradeParams { id = "0", amount = 70_000_000 + i}

params1 :: Integer -> TradeParams
params1 i = TradeParams { id = "1", amount = 70_000_000 + i}

params2 :: Integer -> TradeParams
params2 i = TradeParams { id = "2", amount = 70_000_000 + i}
  
test :: IO ()
test = do
    let dist = Map.fromList [ (wallet 1, Ada.lovelaceValueOf 1_000_000_000
                                      <> Value.singleton assetSymbol2 assetTn0 1
				      <> Value.singleton assetSymbol2 assetTn1 1
				      <> Value.singleton assetSymbol2 assetTn2 1
				      <> Value.singleton assetSymbol1 "Unsigs1" 1
				      <> Value.singleton assetSymbol1 "Unsigs2" 1
				      <> Value.singleton assetSymbol1 "Unsigs0" 1
				      <> Value.singleton assetSymbol1 "startwallet" 1)
                            , (wallet 2, Ada.lovelaceValueOf 1_000_000_000
				      <> Value.singleton assetSymbol1 "wallet2" 1)
                            , (wallet 3, Ada.lovelaceValueOf 000_000_000)
                            , (wallet 4, Ada.lovelaceValueOf 1_000_000_000)
                            , (wallet 5, Ada.lovelaceValueOf 1_000_000_000)
			    , (wallet 6, Ada.lovelaceValueOf 0 
				      <> Value.singleton assetSymbol1 "owner1" 1)
			    , (wallet 7, Ada.lovelaceValueOf 0
				      <> Value.singleton assetSymbol1 "owner2" 1)                           
			    ]
        emCfg = EmulatorConfig (Left dist) def def
    runEmulatorTraceIO' def emCfg $ do
        h1 <- activateContractWallet (wallet 1) Trace.contract
        h2 <- activateContractWallet (wallet 2) Trace.contract
        h3 <- activateContractWallet (wallet 3) Trace.contract
        h4 <- activateContractWallet (wallet 4) Trace.contract
        h5 <- activateContractWallet (wallet 5) Trace.contract
-- offer, sell, buy, cancelOffer, cancelBid, bid, cancelBidAndBuy, cancelOfferAndSell,troll
	void $ Emulator.waitNSlots 1	    	    
	callEndpoint @"init" h1 ()
	void $ Emulator.waitNSlots 1
	callEndpoint @"bid" h2 (params0 10_000) --NFT0
	void $ Emulator.waitNSlots 1
{--	callEndpoint @"sell" h1 (params0 10_000)
	void $ Emulator.waitNSlots 1
	callEndpoint @"offer" h1 (params1 0) --NFT1 
	void $ Emulator.waitNSlots 1
	callEndpoint @"buy" h5 (params1 10000)
	void $ Emulator.waitNSlots 1
	callEndpoint @"buy" h5 (params1 10_000)
	void $ Emulator.waitNSlots 1
	callEndpoint @"bid" h4 (params2 10_000) --NFT2
	void $ Emulator.waitNSlots 1
	callEndpoint @"offer" h1 (params2 1)
	void $ Emulator.waitNSlots 1
	callEndpoint @"cancelBidAndBuy" h4 (params2 10_000)
	void $ Emulator.waitNSlots 1
	callEndpoint @"offer" h5 (params1 400_000_000)
	void $ Emulator.waitNSlots 1
	callEndpoint @"buy" h2 (params1 400_000_000)
	void $ Emulator.waitNSlots 1
--}

contract :: Contract () TradeSchema ContractError ()
contract = endpoints
