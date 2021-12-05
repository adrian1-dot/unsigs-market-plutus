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

module Offchain (endpoints, trade, init, sell, offer, buy, cancelOffer, bid, cancelBid, cancelBidAndBuy, cancelOfferAndSell, troll) where 

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
import           Plutus.Contract      as Contract
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
import           Plutus.ChainIndex.Tx (ChainIndexTx, _citxData)
import           Plutus.Contracts.Currency


import           Types                (ContractInfo (..), TradeSchema (..), TradeParams (..), TradeAction (..), TradeDatum (..), TradeDetails (..))
import           Onchain              (tradeAddress, tradeInstance) 
import           Utility              (contractInfo)




-- Off-Chain


-- helper functions

containsSpaceBudNFT :: Value -> BuiltinByteString -> Bool
containsSpaceBudNFT v tn = valueOf v (policySpaceBudz contractInfo) (TokenName ((prefixSpaceBud contractInfo) <> tn)) >= 1

containsPolicyBidNFT :: Value -> BuiltinByteString -> Bool
containsPolicyBidNFT v tn = valueOf v (policyBid contractInfo) (TokenName ((prefixSpaceBudBid contractInfo) <> tn)) >= 1

policyAssets :: Value -> CurrencySymbol -> [(CurrencySymbol, TokenName, Integer)]
policyAssets v cs = P.filter (\(cs',_,am) -> cs == cs' && am == 1) (flattenValue v)

policyBidLength :: Value -> Integer
policyBidLength v = length $ policyAssets v (policyBid contractInfo)

policyBidRemaining :: Value -> TokenName -> Value
policyBidRemaining v tn = convert (P.filter (\(cs',tn',am) -> (policyBid contractInfo) == cs' && am == 1 && tn /= tn' ) (flattenValue v))
    where convert [] = mempty
          convert ((cs,tn,am):t) = Value.singleton cs tn am <> convert t

lovelacePercentage :: Integer -> Integer -> Integer
lovelacePercentage am p = (am * 10) `Haskell.div` p

--Why trade && endpoints?
trade :: AsContractError e => Contract () TradeSchema e ()
trade = selectList [init, offer, buy, cancelOffer, bid, sell, cancelBid, cancelBidAndBuy, cancelOfferAndSell, troll] >> trade


endpoints :: AsContractError e => Contract () TradeSchema e ()
endpoints = trade

-- init API endpoint is only used in the simulation for getting bid NFTs into circulation
init :: AsContractError e => Promise () TradeSchema e ()
init = endpoint @"init" @() $ \() -> do
    let tx = mustPayToTheScript StartBid (Value.singleton (policyBid contractInfo) (TokenName "SpaceBudBid0") 1 <> Value.singleton (policyBid contractInfo) (TokenName "SpaceBudBid1") 1 <> Value.singleton (policyBid contractInfo) (TokenName "SpaceBudBid2") 1) 
    void $ submitTxConstraints tradeInstance tx


-- this endpoint is just for the simulator to check if a bad actor could buy two SpaceBudz in a single transaction
troll :: AsContractError e => Promise () TradeSchema e ()
troll = endpoint @"troll" @TradeParams $ \(TradeParams{..}) -> do
    utxos <- utxosTxOutTxAt tradeAddress
    utxo <- utxosAt tradeAddress
    pkh <- Contract.ownPubKeyHash
    let offerUtxo0 = [ (oref, txOut, getTradeDatum txOut tx, _ciTxOutValue txOut) | (oref,(txOut, tx)) <- Map.toList utxos, case getTradeDatum txOut tx of (Offer details) -> "0" == budId details && containsSpaceBudNFT (_ciTxOutValue txOut) "0"; _ -> False]
    let offerUtxo1 = [ (oref, txOut, getTradeDatum txOut tx, _ciTxOutValue txOut) | (oref,(txOut, tx)) <- Map.toList utxos, case getTradeDatum txOut tx of (Offer details) -> "1" == budId details && containsSpaceBudNFT (_ciTxOutValue txOut) "1"; _ -> False]
    let offerUtxo0Map = let [(offeroref, offero, Offer offerdetails, offervalue)] = offerUtxo0 in  Map.fromList [(offeroref,offero)]
    let offerUtxo1Map = let [(offeroref, offero, Offer offerdetails, offervalue)] = offerUtxo1 in  Map.fromList [(offeroref,offero)]
    let [(offeroref, offero, Offer offerdetails, offervalue)] = offerUtxo0
    let (owner1PubKeyHash, owner1Fee1, owner1Fee2) = owner1 contractInfo
        (owner2PubKeyHash, owner2Fee1) = owner2 contractInfo
        lovelaceAmount = 500000000
    let (amount1, amount2, amount3) = (lovelacePercentage lovelaceAmount (owner1Fee2),lovelacePercentage lovelaceAmount (owner2Fee1),lovelacePercentage lovelaceAmount (extraRecipient contractInfo))
    
    let tx = collectFromScript offerUtxo0Map Buy <> 
                collectFromScript offerUtxo1Map Buy <> 
                mustPayToPubKey (owner1PubKeyHash) (Ada.lovelaceValueOf amount1) <>
                mustPayToPubKey (owner2PubKeyHash) (Ada.lovelaceValueOf amount2) <>
                mustPayToPubKey (walletPubKeyHash (Emulator.knownWallet 3)) (Ada.lovelaceValueOf amount3) <> -- arbitary address/ knownWallet -> only for simulation?
                mustPayToPubKey (tradeOwner offerdetails) (Ada.lovelaceValueOf (lovelaceAmount - amount1 - amount2 - amount3)) <>
                mustPayToPubKey (pkh) (Value.singleton (policySpaceBudz contractInfo) (TokenName ("SpaceBud0")) 1) <>
                mustPayToPubKey (pkh) (Value.singleton (policySpaceBudz contractInfo) (TokenName ("SpaceBud1")) 1)
    void $ submitTxConstraintsSpending tradeInstance utxo tx


cancelBidAndBuy :: AsContractError e => Promise () TradeSchema e ()
cancelBidAndBuy = endpoint @"cancelBidAndBuy" @TradeParams $ \(TradeParams{..}) -> do
    utxos <- utxosTxOutTxAt tradeAddress
    utxo <- utxosAt tradeAddress
    pkh <- Contract.ownPubKeyHash
    let bidUtxo = [ (oref, txOut, getTradeDatum txOut tx, _ciTxOutValue txOut) | (oref,(txOut, tx)) <- Map.toList utxos, case getTradeDatum txOut tx of (Bid details) -> id == budId details && containsPolicyBidNFT (_ciTxOutValue txOut) id; _ -> False]
    case bidUtxo of
        [(bidoref, bido, Bid biddetails, bidvalue)] -> do
            let offerUtxo = [ (oref, txOut, getTradeDatum txOut tx, _ciTxOutValue txOut) | (oref,(txOut, tx)) <- Map.toList utxos, case getTradeDatum txOut tx of (Offer details) -> id == budId details && containsSpaceBudNFT (_ciTxOutValue txOut) id; _ -> False]
            case offerUtxo of
                [(offeroref, offero, Offer offerdetails, offervalue)] -> do
                    let (owner1PubKeyHash, owner1Fee1, owner1Fee2) = owner1 contractInfo
                        (owner2PubKeyHash, owner2Fee1) = owner2 contractInfo
                        lovelaceAmount = requestedAmount offerdetails
                    if lovelaceAmount >= 400000000 then do
                        let (amount1, amount2, amount3) = (lovelacePercentage lovelaceAmount (owner1Fee2),lovelacePercentage lovelaceAmount (owner2Fee1),lovelacePercentage lovelaceAmount (extraRecipient contractInfo))
                        let bidUtxoMap = Map.fromList [(bidoref,bido)]
                        let offerUtxoMap = Map.fromList [(offeroref,offero)]
                        let tx = collectFromScript bidUtxoMap Cancel <> 
                                collectFromScript offerUtxoMap Buy <> 
                                mustPayToPubKey (owner1PubKeyHash) (Ada.lovelaceValueOf amount1) <>
                                mustPayToPubKey (owner2PubKeyHash) (Ada.lovelaceValueOf amount2) <>
                                mustPayToPubKey (walletPubKeyHash (Emulator.knownWallet 3)) (Ada.lovelaceValueOf amount3) <> -- arbitary address
                                mustPayToPubKey (tradeOwner offerdetails) (Ada.lovelaceValueOf (lovelaceAmount - amount1 - amount2 - amount3)) <>
                                mustPayToPubKey (pkh) (Value.singleton (policySpaceBudz contractInfo) (TokenName ("SpaceBud" <> id)) 1) <>
                                mustPayToTheScript StartBid (Value.singleton (policyBid contractInfo) (TokenName ("SpaceBudBid" <> id)) 1)
                        void $ submitTxConstraintsSpending tradeInstance utxo tx
                    else do
                        let amount1 = lovelacePercentage lovelaceAmount (owner1Fee1)
                        let bidUtxoMap = Map.fromList [(bidoref,bido)]
                        let offerUtxoMap = Map.fromList [(offeroref,offero)]
                        let tx = collectFromScript bidUtxoMap Cancel <> 
                                collectFromScript offerUtxoMap Buy <> 
                                mustPayToPubKey (owner1PubKeyHash) (Ada.lovelaceValueOf amount1) <>
                                mustPayToPubKey (tradeOwner offerdetails) (Ada.lovelaceValueOf (lovelaceAmount - amount1)) <>
                                mustPayToPubKey (pkh) (Value.singleton (policySpaceBudz contractInfo) (TokenName ("SpaceBud" <> id)) 1) <>
                                mustPayToTheScript StartBid (Value.singleton (policyBid contractInfo) (TokenName ("SpaceBudBid" <> id)) 1)
                        void $ submitTxConstraintsSpending tradeInstance utxo tx
                _ -> traceError "expected only one output"
        _ -> traceError "expected only one output"

cancelOfferAndSell :: AsContractError e => Promise () TradeSchema e ()
cancelOfferAndSell = endpoint @"cancelOfferAndSell" @TradeParams $ \(TradeParams{..}) -> do
    utxos <- utxosTxOutTxAt tradeAddress
    utxo <- utxosAt tradeAddress
    pkh <- Contract.ownPubKeyHash
    let offerUtxo = [ (oref, txOut, getTradeDatum txOut tx, _ciTxOutValue txOut) | (oref,(txOut, tx)) <- Map.toList utxos, case getTradeDatum txOut tx of (Offer details) -> id == budId details && containsSpaceBudNFT (_ciTxOutValue txOut) id; _ -> False]
    case offerUtxo of
        [(offeroref, offero, Offer offerdetails, offervalue)] -> do
            let bidUtxo = [ (oref, txOut, getTradeDatum txOut tx, _ciTxOutValue txOut) | (oref, (txOut, tx)) <- Map.toList utxos, case getTradeDatum txOut tx of (Bid details) -> id == budId details && containsPolicyBidNFT (_ciTxOutValue txOut) id; _ -> False]
            case bidUtxo of
                [(bidoref, bido, Bid biddetails, bidvalue)] -> do
                    let (owner1PubKeyHash, owner1Fee1, owner1Fee2) = owner1 contractInfo
                        (owner2PubKeyHash, owner2Fee1) = owner2 contractInfo
                        lovelaceAmount = getLovelace $ Ada.fromValue bidvalue
                    if lovelaceAmount >= 400000000 then do
                        let (amount1, amount2, amount3) = (lovelacePercentage lovelaceAmount (owner1Fee2),lovelacePercentage lovelaceAmount (owner2Fee1),lovelacePercentage lovelaceAmount (extraRecipient contractInfo))
                            bidUtxoMap = Map.fromList [(bidoref,bido)]
                            offerUtxoMap = Map.fromList [(offeroref,offero)]
                            tx = collectFromScript bidUtxoMap Sell <> 
                                collectFromScript offerUtxoMap Cancel <>
                                mustPayToPubKey (owner1PubKeyHash) (Ada.lovelaceValueOf amount1) <>
                                mustPayToPubKey (owner2PubKeyHash) (Ada.lovelaceValueOf amount2) <>
                                mustPayToPubKey (walletPubKeyHash (Emulator.knownWallet 3)) (Ada.lovelaceValueOf amount3) <> -- arbitary address
                                mustPayToPubKey (pkh) (Ada.lovelaceValueOf (lovelaceAmount - amount1 - amount2 - amount3)) <>
                                mustPayToPubKey (tradeOwner biddetails) (Value.singleton (policySpaceBudz contractInfo) (TokenName ("SpaceBud" <> id)) 1) <>
                                mustPayToTheScript StartBid (Value.singleton (policyBid contractInfo) (TokenName ("SpaceBudBid" <> id)) 1)
                        void $ submitTxConstraintsSpending tradeInstance utxo tx
                    else do
                        let amount1 = lovelacePercentage lovelaceAmount (owner1Fee1)
                            bidUtxoMap = Map.fromList [(bidoref,bido)]
                            offerUtxoMap = Map.fromList [(offeroref,offero)]
                            tx = collectFromScript bidUtxoMap Sell <> 
                                collectFromScript offerUtxoMap Cancel <>
                                mustPayToPubKey (owner1PubKeyHash) (Ada.lovelaceValueOf amount1) <>
                                mustPayToPubKey (pkh) (Ada.lovelaceValueOf (lovelaceAmount - amount1)) <>
                                mustPayToPubKey (tradeOwner biddetails) (Value.singleton (policySpaceBudz contractInfo) (TokenName ("SpaceBud" <> id)) 1) <>
                                mustPayToTheScript StartBid (Value.singleton (policyBid contractInfo) (TokenName ("SpaceBudBid" <> id)) 1)
                        void $ submitTxConstraintsSpending tradeInstance utxo tx
                _ -> traceError "expected only one output"
        _ -> traceError "expected only one output"

bid :: AsContractError e => Promise () TradeSchema e ()
bid = endpoint @"bid" @TradeParams $ \(TradeParams{..}) -> do
    utxos <- utxosTxOutTxAt tradeAddress
    utxo <- utxosAt tradeAddress
    pkh <- Contract.ownPubKeyHash
    let bidUtxo = [ (oref, txOut, getTradeDatum txOut tx, _ciTxOutValue txOut) | (oref, (txOut, tx)) <- Map.toList utxos, case getTradeDatum txOut tx of (StartBid) -> containsPolicyBidNFT (_ciTxOutValue txOut) id; (Bid details) -> budId details == id && containsPolicyBidNFT (_ciTxOutValue txOut) id; _ -> False]
    let bidDatum = Bid TradeDetails {budId = id, requestedAmount = 1, tradeOwner = pkh}
    case bidUtxo of
        [(oref, o, StartBid, value)] -> do
            if amount < minPrice contractInfo then traceError "amount too small" else if policyBidLength value > 1 then do
                let utxoMap = Map.fromList [(oref,o)]
                    tx = collectFromScript utxoMap BidHigher <> 
                        mustPayToTheScript bidDatum (Ada.lovelaceValueOf (amount) <> Value.singleton (policyBid contractInfo) (TokenName ("SpaceBudBid" <> id)) 1) <>
                        mustPayToTheScript StartBid (policyBidRemaining value (TokenName ("SpaceBudBid" <> id)))
                void $ submitTxConstraintsSpending tradeInstance utxo tx
            else do
                let utxoMap = Map.fromList [(oref,o)]
                    tx = collectFromScript utxoMap BidHigher <> 
                        mustPayToTheScript bidDatum (Ada.lovelaceValueOf (amount) <> Value.singleton (policyBid contractInfo) (TokenName ("SpaceBudBid" <> id)) 1)
                void $ submitTxConstraintsSpending tradeInstance utxo tx
        [(oref, o, Bid details, value)] -> do
            if amount < bidStep contractInfo + getLovelace (Ada.fromValue value) then traceError "amount too small" else do
                let utxoMap = Map.fromList [(oref,o)]
                    tx = collectFromScript utxoMap BidHigher <> 
                        mustPayToTheScript bidDatum (Ada.lovelaceValueOf (amount) <> Value.singleton (policyBid contractInfo) (TokenName ("SpaceBudBid" <> id)) 1) <>
                        case (tradeOwner details == pkh) of True -> mempty; False -> mustPayToPubKey (tradeOwner details) (Ada.toValue (Ada.fromValue value))
                void $ submitTxConstraintsSpending tradeInstance utxo tx
        _ -> traceError "expected only one output"


sell :: AsContractError e => Promise () TradeSchema e ()
sell = endpoint @"sell" @TradeParams $ \(TradeParams{..}) -> do
    utxos <- utxosTxOutTxAt tradeAddress
    utxo <- utxosAt tradeAddress
    pkh <- Contract.ownPubKeyHash
    let bidUtxo = [ (oref, txOut, getTradeDatum txOut tx, _ciTxOutValue txOut) | (oref,(txOut, tx)) <- Map.toList utxos, case getTradeDatum txOut tx of (Bid details) -> id == budId details && containsPolicyBidNFT (_ciTxOutValue txOut) id; _ -> False]
    case bidUtxo of
        [(oref, o, Bid details, value)] -> do
            let (owner1PubKeyHash, owner1Fee1, owner1Fee2) = owner1 contractInfo
                (owner2PubKeyHash, owner2Fee1) = owner2 contractInfo
                lovelaceAmount = getLovelace $ Ada.fromValue value
            if lovelaceAmount >= 400000000 then do
                let (amount1, amount2, amount3) = (lovelacePercentage lovelaceAmount (owner1Fee2),lovelacePercentage lovelaceAmount (owner2Fee1),lovelacePercentage lovelaceAmount (extraRecipient contractInfo))
                    utxoMap = Map.fromList [(oref,o)]
                    tx = collectFromScript utxoMap Sell <> 
                        mustPayToPubKey (owner1PubKeyHash) (Ada.lovelaceValueOf amount1) <>
                        mustPayToPubKey (owner2PubKeyHash) (Ada.lovelaceValueOf amount2) <>
                        mustPayToPubKey ( walletPubKeyHash (Emulator.knownWallet 3)) (Ada.lovelaceValueOf amount3) <> -- arbitary address
                        mustPayToPubKey (pkh) (Ada.lovelaceValueOf (lovelaceAmount - amount1 - amount2 - amount3)) <>
                        mustPayToPubKey (tradeOwner details) (Value.singleton (policySpaceBudz contractInfo) (TokenName ("SpaceBud" <> id)) 1) <>
                        mustPayToTheScript StartBid (Value.singleton (policyBid contractInfo) (TokenName ("SpaceBudBid" <> id)) 1)
                void $ submitTxConstraintsSpending tradeInstance utxo tx
            else do
                let amount1 = lovelacePercentage lovelaceAmount (owner1Fee1)
                    utxoMap = Map.fromList [(oref,o)]
                    tx = collectFromScript utxoMap Sell <> 
                        mustPayToPubKey (owner1PubKeyHash) (Ada.lovelaceValueOf amount1) <>
                        mustPayToPubKey (pkh) (Ada.lovelaceValueOf (lovelaceAmount - amount1)) <>
                        mustPayToPubKey (tradeOwner details) (Value.singleton (policySpaceBudz contractInfo) (TokenName ("SpaceBud" <> id)) 1) <>
                        mustPayToTheScript StartBid (Value.singleton (policyBid contractInfo) (TokenName ("SpaceBudBid" <> id)) 1)
                void $ submitTxConstraintsSpending tradeInstance utxo tx
        _ -> traceError "expected only one output"


offer :: AsContractError e => Promise () TradeSchema e ()
offer = endpoint @"offer" @TradeParams $ \(TradeParams{..}) -> do
    if amount < minPrice contractInfo then traceError "amount too small" else do
        pkh <- Contract.ownPubKeyHash
        let tradeDatum = Offer TradeDetails {budId = id, requestedAmount = amount, tradeOwner = pkh}
            tx = mustPayToTheScript tradeDatum (Value.singleton (policySpaceBudz contractInfo) (TokenName ("SpaceBud" <> id)) 1)
        void $ submitTxConstraints tradeInstance tx


buy :: AsContractError e => Promise () TradeSchema e ()
buy = endpoint @"buy" @TradeParams $ \(TradeParams{..}) -> do
    utxos <- utxosTxOutTxAt tradeAddress
    utxo <- utxosAt tradeAddress
    pkh <- Contract.ownPubKeyHash
    let offerUtxo = [ (oref, txOut, getTradeDatum txOut tx, _ciTxOutValue txOut) | (oref, (txOut, tx)) <- Map.toList utxos, case getTradeDatum txOut tx of (Offer details) -> id == budId details && containsSpaceBudNFT (_ciTxOutValue txOut) id; _ -> False]
    case offerUtxo of
        [(oref, o, Offer details, value)] -> do
            let (owner1PubKeyHash, owner1Fee1, owner1Fee2) = owner1 contractInfo
                (owner2PubKeyHash, owner2Fee1) = owner2 contractInfo
                lovelaceAmount = requestedAmount details
            if lovelaceAmount >= 400000000 then do
                let (amount1, amount2, amount3) = (lovelacePercentage lovelaceAmount (owner1Fee2),lovelacePercentage lovelaceAmount (owner2Fee1),lovelacePercentage lovelaceAmount (extraRecipient contractInfo))
                let utxoMap = Map.fromList [(oref,o)]
                let tx = collectFromScript utxoMap Buy <> 
                        mustPayToPubKey (owner1PubKeyHash) (Ada.lovelaceValueOf amount1) <>
                        mustPayToPubKey (owner2PubKeyHash) (Ada.lovelaceValueOf amount2) <>
                        mustPayToPubKey ( walletPubKeyHash (Emulator.knownWallet 3)) (Ada.lovelaceValueOf amount3) <> -- arbitary address
                        mustPayToPubKey (tradeOwner details) (Ada.lovelaceValueOf (lovelaceAmount - amount1 - amount2 - amount3)) <>
                        mustPayToPubKey (pkh) (Value.singleton (policySpaceBudz contractInfo) (TokenName ("SpaceBud" <> id)) 1)
                void $ submitTxConstraintsSpending tradeInstance utxo tx
            else do
                let amount1 = lovelacePercentage lovelaceAmount (owner1Fee1)
                let utxoMap = Map.fromList [(oref,o)]
                let tx = collectFromScript utxoMap Buy <> 
                        mustPayToPubKey (owner1PubKeyHash) (Ada.lovelaceValueOf amount1) <>
                        mustPayToPubKey (tradeOwner details) (Ada.lovelaceValueOf (lovelaceAmount - amount1)) <>
                        mustPayToPubKey (pkh) (Value.singleton (policySpaceBudz contractInfo) (TokenName ("SpaceBud" <> id)) 1)
                void $ submitTxConstraintsSpending tradeInstance utxo tx
        _ -> traceError "expected only one output"


cancelOffer :: AsContractError e => Promise () TradeSchema e ()
cancelOffer = endpoint @"cancelOffer" @TradeParams $ \(TradeParams{..}) -> do
    utxos <- utxosTxOutTxAt tradeAddress
    utxo <- utxosAt tradeAddress
    pkh <- Contract.ownPubKeyHash
    let offerUtxo = [ (oref, txOut, getTradeDatum txOut tx, _ciTxOutValue txOut) | (oref,(txOut, tx)) <- Map.toList utxos, case getTradeDatum txOut tx of (Offer details) -> id == budId details && containsSpaceBudNFT (_ciTxOutValue txOut) id; _ -> False]
    case offerUtxo of
        [(oref, o, Offer details, value)] -> do
            let utxoMap = Map.fromList [(oref,o)]
                tx = collectFromScript utxoMap (Cancel) <> 
                    mustPayToPubKey (pkh) value
            void $ submitTxConstraintsSpending tradeInstance utxo tx
        _ -> traceError "expected only one output"

cancelBid :: AsContractError e => Promise () TradeSchema e ()
cancelBid = endpoint @"cancelBid" @TradeParams $ \(TradeParams{..}) -> do
    utxos <- utxosTxOutTxAt tradeAddress
    utxo <- utxosAt tradeAddress
    pkh <- Contract.ownPubKeyHash
    let bidUtxo = [ (oref, txOut, getTradeDatum txOut tx, _ciTxOutValue txOut) | (oref, (txOut, tx)) <- Map.toList utxos, case getTradeDatum txOut tx of (Bid details) -> id == budId details && containsPolicyBidNFT (_ciTxOutValue txOut) id; _ -> False]
    case bidUtxo of
        [(oref, o, Bid details, value)] -> do
            let utxoMap = Map.fromList [(oref,o)]
                tx = collectFromScript utxoMap (Cancel) <> 
                    mustPayToPubKey (tradeOwner details) (Ada.toValue (Ada.fromValue value)) <>
                    mustPayToTheScript StartBid (Value.singleton (policyBid contractInfo) (TokenName ("SpaceBudBid" <> id)) 1)
            
            void $ submitTxConstraintsSpending tradeInstance utxo tx
        _ -> traceError "expected only one output"

{--  *OLD*
 getTradeDatum :: TxOutTx -> TradeDatum
getTradeDatum o = case txOutDatum (txOutTxOut o) of
    Just h -> do
        let [(_,datum)] = P.filter (\(h',_) -> h == h') (Map.toList (txData (txOutTxTx o)))
        let parsedDatum = PlutusTx.fromBuiltinData (getDatum datum) :: Maybe TradeDatum
        case parsedDatum of
            Just b -> b
            _ -> traceError "expected datum"
    _ -> traceError "expected datum"
--}

--should Work? 
getTradeDatum :: ChainIndexTxOut -> ChainIndexTx -> TradeDatum
getTradeDatum o t = case txOutDatum (toTxOut o) of
    Just h -> do
        let [(_,datum)] = P.filter (\(h',_) -> h == h') (Map.toList (_citxData t))
        let parsedDatum = PlutusTx.fromBuiltinData (getDatum datum) :: Maybe TradeDatum
        case parsedDatum of
            Just b -> b
            _ -> traceError "expected datum"
    _ -> traceError "expected datum"


--Only for Playground 
spacebud0 = KnownCurrency (ValidatorHash "f") "Token" (TokenName "SpaceBud0" :| [])
spacebud1 = KnownCurrency (ValidatorHash "f") "Token" (TokenName "SpaceBud1" :| [])
spacebud2 = KnownCurrency (ValidatorHash "f") "Token" (TokenName "SpaceBud2" :| [])

spacebudBid0 = KnownCurrency (ValidatorHash "f") "Token" (TokenName "SpaceBudBid0" :| [])
spacebudBid1 = KnownCurrency (ValidatorHash "f") "Token" (TokenName "SpaceBudBid1" :| [])
spacebudBid2 = KnownCurrency (ValidatorHash "f") "Token" (TokenName "SpaceBudBid2" :| [])


