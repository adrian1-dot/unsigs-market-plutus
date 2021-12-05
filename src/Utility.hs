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


module Utility (contractInfo, prefixToken, prefixBidToken, wallet, assetSymbol1, assetSymbol2) where 


import           Plutus.V1.Ledger.Crypto (PubKeyHash)
import           Ledger (pubKeyHash)
import Plutus.V1.Ledger.Value (CurrencySymbol(..), TokenName(..), Value(..))
import           Wallet.Emulator.Types (WalletNumber (..), fromWalletNumber, Wallet (..))
import           Wallet.Emulator.Wallet (Wallet, walletPubKeyHash)
import           PlutusTx.Prelude ((.), BuiltinByteString)
import           PlutusTx.Builtins.Internal (BuiltinByteString (..), encodeUtf8, BuiltinString)
import           PlutusTx.Builtins.Class 
import           Prelude hiding ((.))

import Types (ContractInfo (..))


wallet :: Integer -> Wallet 
wallet = fromWalletNumber . WalletNumber 

owner1Pkh :: PubKeyHash
owner1Pkh = walletPubKeyHash $ wallet 1

owner2Pkh :: PubKeyHash 
owner2Pkh = walletPubKeyHash $ wallet 2

assetSymbol1 :: CurrencySymbol
assetSymbol1 = CurrencySymbol {unCurrencySymbol= "policySpaceBudz"}

prefixToken :: BuiltinByteString
prefixToken = encodeUtf8 $ stringToBuiltinString "SpaceBud"

assetSymbol2 :: CurrencySymbol
assetSymbol2 = CurrencySymbol {unCurrencySymbol = "policyBid"}

prefixBidToken :: BuiltinByteString
prefixBidToken = encodeUtf8 $ stringToBuiltinString "SpaceBudBid"

contractInfo :: ContractInfo
contractInfo = ContractInfo 
    { policySpaceBudz = assetSymbol1
    , policyBid = assetSymbol2
    , prefixSpaceBud = prefixToken
    , prefixSpaceBudBid = prefixBidToken
    , owner1 = ( owner1Pkh, 416, 625) -- 2.4% 1.6%
    , owner2 = ( owner2Pkh, 2500) -- 0.4%
    , extraRecipient = 2500 -- 0.4%
    , minPrice = 70000000
    , bidStep = 10000
    }

contractInfoSpace :: ContractInfo
contractInfoSpace = ContractInfo 
    { policySpaceBudz = "d5e6bf0500378d4f0da4e8dde6becec7621cd8cbf5cbb9b87013d4cc"
    , policyBid = "800df05a0cc6b6f0d28aaa1812135bd9eebfbf5e8e80fd47da9989eb"
    , prefixSpaceBud = "SpaceBud"
    , prefixSpaceBudBid = "SpaceBudBid"
    , owner1 = ("826d9fafe1b3acf15bd250de69c04e3fc92c4493785939e069932e89", 416, 625) -- 2.4% 1.6%
    , owner2 = ("88269f8b051a739300fe743a7b315026f4614ce1216a4bb45d7fd0f5", 2500) -- 0.4%
    , extraRecipient = 2500 -- 0.4%
    , minPrice = 70000000
    , bidStep = 10000
    }


