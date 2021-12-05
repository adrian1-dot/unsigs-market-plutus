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


module Types ( 
	ContractInfo (..), 
	TradeDetails (..), 
	TradeDatum (..), 
	TradeSchema, 
	TradeParams (..), 
	Trade (..),
	TradeAction (..)
	) where


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



data ContractInfo = ContractInfo
    { policySpaceBudz :: !CurrencySymbol
    , policyBid :: !CurrencySymbol
    , prefixSpaceBud :: !BuiltinByteString
    , prefixSpaceBudBid :: !BuiltinByteString
    , owner1 :: !(PubKeyHash, Integer, Integer)
    , owner2 :: !(PubKeyHash, Integer)
    , extraRecipient :: !Integer
    , minPrice :: !Integer
    , bidStep :: !Integer
    } deriving (Generic, ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''ContractInfo [('ContractInfo , 0)]
PlutusTx.makeLift ''ContractInfo

-- Data and Redeemers

data TradeDetails = TradeDetails
    { tradeOwner :: !PubKeyHash
    , budId :: !BuiltinByteString
    , requestedAmount :: !Integer
    } deriving (Generic, ToJSON, FromJSON)

instance Eq TradeDetails where
    {-# INLINABLE (==) #-}
    -- tradeOwner is not compared, since tradeOwner can changes with each trade/higher bid
    a == b = (budId  a == budId  b) &&
             (requestedAmount a == requestedAmount b)

PlutusTx.makeIsDataIndexed ''TradeDetails [ ('TradeDetails, 0)]
PlutusTx.makeLift ''TradeDetails


data TradeDatum = StartBid | Bid TradeDetails | Offer TradeDetails 
    deriving (Generic, ToJSON, FromJSON)

-- only compare necessary types in order to save storage!!
instance Eq TradeDatum where
    {-# INLINABLE (==) #-}
    StartBid == StartBid = True
    Bid a == Bid b = a == b

PlutusTx.makeIsDataIndexed ''TradeDatum [ ('StartBid, 0)
                                        , ('Bid,   1)
                                        , ('Offer, 2)
                                        ]
PlutusTx.makeLift ''TradeDatum


data TradeAction = Buy | Sell | BidHigher | Cancel
    deriving (Generic, ToJSON, FromJSON)

 
PlutusTx.makeIsDataIndexed ''TradeAction [ ('Buy,       0)
                                         , ('Sell,      1)
                                         , ('BidHigher, 2)
                                         , ('Cancel,    3)
                                        ]
PlutusTx.makeLift ''TradeAction

    
data Trade
instance Scripts.ValidatorTypes Trade where
    type instance RedeemerType Trade = TradeAction
    type instance DatumType Trade = TradeDatum

--why not String?
data TradeParams = TradeParams
    { id :: !BuiltinByteString
    , amount :: !Integer
    } deriving (Generic, Show, ToJSON, FromJSON, ToSchema)


type TradeSchema = Endpoint "offer" TradeParams
        .\/ Endpoint "buy" TradeParams
        .\/ Endpoint "cancelOffer" TradeParams
        .\/ Endpoint "cancelBid" TradeParams
        .\/ Endpoint "init" ()
        .\/ Endpoint "bid" TradeParams
        .\/ Endpoint "sell" TradeParams
        .\/ Endpoint "cancelBidAndBuy" TradeParams
        .\/ Endpoint "cancelOfferAndSell" TradeParams
        .\/ Endpoint "troll" TradeParams


mkSchemaDefinitions ''TradeSchema
