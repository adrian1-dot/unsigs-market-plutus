{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE BangPatterns               #-}


module Onchain (tradeValidator, tradeInstance, tradeAddress, tradeInstance) where 


import Playground.Contract
import Wallet.Emulator.Wallet as Emulator
import Plutus.Contract
import           Data.Map             as Map
import qualified Prelude              as Haskell
--
import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON,encode)
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Text            (pack, Text)
import           GHC.Generics         (Generic)
import qualified PlutusTx
import           PlutusTx.Prelude     as P
import           Ledger               hiding (singleton)
import           Ledger.Credential    (Credential (..))
import           Ledger.Constraints   as Constraints
import qualified Ledger.Scripts       as Scripts
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         as Value
import           Ledger.Ada           as Ada hiding (divide)
import           Prelude              ((/), Float, toInteger, floor)
import           Text.Printf          (printf)
import qualified PlutusTx.AssocMap    as AssocMap
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy  as LBS
import           Cardano.Api hiding (Value, TxOut)
import           Cardano.Api.Shelley hiding (Value, TxOut)
import           Codec.Serialise hiding (encode)
import qualified Plutus.V1.Ledger.Api as Plutus

import           Types                (ContractInfo (..), TradeAction (..), TradeDatum (..), Trade, TradeDetails (..))
import           Utility              (contractInfo)


-- easier way?
toFraction :: Float -> Integer
toFraction p = toInteger $ floor (1 / (p / 1000))


-- Validator

{-# INLINABLE tradeValidate #-}
tradeValidate :: ContractInfo -> TradeDatum -> TradeAction -> ScriptContext -> Bool
tradeValidate contractInfo@ContractInfo{..} tradeDatum tradeAction context = case tradeDatum of
    StartBid -> case tradeAction of
        BidHigher -> correctStartBidOutputs

    Bid details@TradeDetails{..} -> case tradeAction of
        BidHigher -> 
            Bid details == scriptOutputDatum && -- expected correct script output datum
            Ada.fromValue (scriptInputValue) + Ada.lovelaceOf bidStep <= Ada.fromValue scriptOutputValue && -- expected correct bid amount
            containsPolicyBidNFT scriptOutputValue nftId && -- expected correct bidPolicy NFT
            case txInfo `txSignedBy` tradeOwner of True -> True; False -> Ada.fromValue (valuePaidTo txInfo tradeOwner) >= Ada.fromValue scriptInputValue -- expected previous bidder refund
        Sell -> 
            scriptOutputDatum == StartBid && -- expected correct script output datum
            containsPolicyBidNFT scriptOutputValue nftId && -- expected correct bidPolicy NFT
            containsUnsigsNFT (valuePaidTo txInfo tradeOwner) nftId && -- expected bidder to be paid
            correctSplit (getLovelace (Ada.fromValue scriptInputValue)) signer -- expected ada to be split correctly
        Cancel -> 
            txInfo `txSignedBy` tradeOwner && -- expected correct owner
            scriptOutputDatum == StartBid && -- expected correct script output datum
            containsPolicyBidNFT scriptOutputValue nftId -- expected correct bidPolicy NFT

    Offer TradeDetails{..} -> case tradeAction of
        Buy ->
            containsUnsigsNFT (valuePaidTo txInfo signer) nftId && -- expected buyer to be paid
            requestedAmount >= minPrice && -- expected at least minPrice buy
            correctSplit requestedAmount tradeOwner -- expected ada to be split correctly
        Cancel -> 
            txInfo `txSignedBy` tradeOwner -- expected correct owner

    where
        txInfo :: TxInfo
        txInfo = scriptContextTxInfo context

        policyAssets :: Value -> CurrencySymbol -> [(CurrencySymbol, TokenName, Integer)]
        policyAssets v cs = P.filter (\(cs',_,am) -> cs == cs' && am == 1) (flattenValue v)

        signer :: PubKeyHash
        signer = case txInfoSignatories txInfo of
            [pubKeyHash] -> pubKeyHash

        (owner1PubKeyHash, owner1Fee1, owner1Fee2) = owner1
        (owner2PubKeyHash, owner2Fee1) = owner2

        -- minADA requirement forces the contract to give up certain fee recipients
        correctSplit :: Integer -> PubKeyHash -> Bool
        correctSplit lovelaceAmount tradeRecipient
            | lovelaceAmount >= 400000000 = let (amount1, amount2, amount3) = (lovelacePercentage lovelaceAmount (owner1Fee2),lovelacePercentage lovelaceAmount (owner2Fee1),lovelacePercentage lovelaceAmount extraRecipient) 
                in 
                  Ada.fromValue (valuePaidTo txInfo owner1PubKeyHash) >= Ada.lovelaceOf amount1 && -- expected owner1 to receive right amount
                  Ada.fromValue (valuePaidTo txInfo owner2PubKeyHash) >= Ada.lovelaceOf amount2 && -- expected owner2 to receive right amount
                  Ada.fromValue (valuePaidTo txInfo tradeRecipient) >= Ada.lovelaceOf (lovelaceAmount - amount1 - amount2 - amount3) -- expected trade recipient to receive right amount
            | otherwise = let amount1 = lovelacePercentage lovelaceAmount (owner1Fee1)
                in
                  Ada.fromValue (valuePaidTo txInfo owner1PubKeyHash) >= Ada.lovelaceOf amount1 && -- expected owner1 to receive right amount
                  Ada.fromValue (valuePaidTo txInfo tradeRecipient) >= Ada.lovelaceOf (lovelaceAmount - amount1) -- expected trade recipient to receive right amount
          
        lovelacePercentage :: Integer -> Integer -> Integer
        lovelacePercentage am p = (am * 10) `divide` p


        outputInfo :: TxOut -> (Value, TradeDatum)
        outputInfo o = case txOutAddress o of
            Address (ScriptCredential _) _  -> case txOutDatumHash o of
                Just h -> case findDatum h txInfo of
                    Just (Datum d) ->  case PlutusTx.fromBuiltinData d of
                        Just b -> (txOutValue o, b)

        policyBidLength :: Value -> Integer
        policyBidLength v = length $ policyAssets v policyBid

        containsPolicyBidNFT :: Value -> BuiltinByteString -> Bool
        containsPolicyBidNFT v tn = valueOf v policyBid (TokenName (prefixUnsigsBid <> tn)) >= 1

        containsUnsigsNFT :: Value -> BuiltinByteString -> Bool
        containsUnsigsNFT v tn = valueOf v policyUnsigs (TokenName (prefixUnsigs <> tn)) >= 1

        -- eager evaluation to check correct script inputs for all branches!!     
        scriptInputValue :: Value
        !scriptInputValue =
            let
                isScriptInput i = case (txOutDatumHash . txInInfoResolved) i of
                    Nothing -> False
                    Just _  -> True
                xs = [ txOutValue (txInInfoResolved i)  | i <- txInfoInputs txInfo, isScriptInput i]
            in
                case xs of
                    [v] -> v -- normally just one script input is allowed
                    [v1, v2] -> 
                        -- allow 2 script inputs if it's a combination of cancelling bid and buying OR cancelling offer and selling (same Unsigs only)
                        case tradeDatum of
                            (Bid TradeDetails{..}) -> case (containsPolicyBidNFT v1 nftId, containsUnsigsNFT v2 nftId) of
                                    (True, True) -> v1 -- expected script input 1 to contain bid token and script input 2 Unsigs 
                                    (False, False) -> v2 -- expected script input 2 to contain bid token and script input 1 Unsigs
                            (Offer TradeDetails{..}) -> case (containsUnsigsNFT v1 nftId, containsPolicyBidNFT v2 nftId) of
                                (True, True) -> v1 -- expected script input 2 to contain bid token and script input 1 Unsigs 
                                (False, False) -> v2 -- expected script input 1 to contain bid token and script input 2 Unsigs
                                 

        scriptOutputValue :: Value
        scriptOutputDatum :: TradeDatum
        (scriptOutputValue, scriptOutputDatum) = case getContinuingOutputs context of
            [o] -> outputInfo o

        -- 2 outputs possible because of distribution of inital bid NFT tokens and only applies if datum is StartBid
        correctStartBidOutputs :: Bool
        correctStartBidOutputs = if policyBidLength scriptInputValue > 1 
            then 
                case getContinuingOutputs context of
                    [o1, o2] -> let (info1, info2) = (outputInfo o1, outputInfo o2) in
                                case info1 of
                                    (v1, StartBid) -> 
                                        policyBidLength scriptInputValue - 1 == policyBidLength v1 && -- expected correct policyBid NFTs amount in output
                                        case info2 of
                                            (v2, Bid TradeDetails{..}) ->
                                                containsPolicyBidNFT v2 nftId && -- expected policyBid NFT in output
                                                getLovelace (Ada.fromValue v2) >= minPrice && -- expected at least minPrice bid
                                                requestedAmount == 1 -- expeced correct output datum amount
                                    (v1, Bid TradeDetails{..}) -> 
                                        containsPolicyBidNFT v1 nftId && -- expected policyBid NFT in output
                                        getLovelace (Ada.fromValue v1) >= minPrice && -- expected at least minPrice bid
                                        requestedAmount == 1 && -- expeced correct output datum amount
                                        case info2 of
                                            (v2, StartBid) -> 
                                                policyBidLength scriptInputValue - 1 == policyBidLength v2 -- expect correct policyBid NFTs amount in output
            else
                case getContinuingOutputs context of
                    [o] -> let (value, datum) = outputInfo o in case datum of
                            (Bid TradeDetails{..}) ->
                                containsPolicyBidNFT value nftId && -- expected policyBid NFT in output
                                getLovelace (Ada.fromValue value) >= minPrice && -- expected at least minPrice bid
                                requestedAmount == 1 -- expeced correct output datum amount

        


tradeInstance :: Scripts.TypedValidator Trade
tradeInstance = Scripts.mkTypedValidator @Trade
    ($$(PlutusTx.compile [|| tradeValidate ||]) `PlutusTx.applyCode` PlutusTx.liftCode contractInfo)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @TradeDatum @TradeAction

tradeValidator ::  Validator
tradeValidator = Scripts.validatorScript tradeInstance


tradeAddress ::  Ledger.Address
tradeAddress = scriptAddress tradeValidator

-- Serialization

{-
    As a Script
-}

tradeScript :: Plutus.Script
tradeScript = Plutus.unValidatorScript tradeValidator

{-
    As a Short Byte String
-}

tradeSBS :: SBS.ShortByteString
tradeSBS =  SBS.toShort . LBS.toStrict $ serialise tradeScript

{-
    As a Serialised Script
-}

tradeSerialised :: PlutusScript PlutusScriptV1
tradeSerialised = PlutusScriptSerialised tradeSBS













