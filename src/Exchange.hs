{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts    	#-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude   	#-}
{-# LANGUAGE ScopedTypeVariables 	#-}
{-# LANGUAGE TemplateHaskell     	#-}
{-# LANGUAGE TypeApplications    	#-}
{-# LANGUAGE TypeFamilies        	#-}
{-# LANGUAGE TypeOperators       	#-}
{-# LANGUAGE RecordWildCards     	#-}

module Exchange
  ( exchange
  , exchangeShortBs
  ) where

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1) 
import           Codec.Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

import           Plutus.V1.Ledger.Contexts
import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Plutus.V1.Ledger.Address
import           Plutus.V1.Ledger.Scripts
import           Plutus.V1.Ledger.Api
import           Plutus.V1.Ledger.Value
import qualified PlutusTx
import           PlutusTx.Prelude hiding (Semigroup (..), unless)
import           Prelude              (IO, Semigroup (..), Show (..), String)

import           Ledger.Typed.Scripts as Scripts

data ExchangeRedeemer = ExchangeRedeemer
    { beneficiary :: !PubKeyHash
    }

data ExchangeDatum = ExchangeDatum
    { number :: Integer
    }

data ContractInfo = ContractInfo
    { secretHash             :: !DatumHash
    , oldAadaPolicyID        :: !CurrencySymbol
    , newAadaPolicyID        :: !CurrencySymbol
    , aadaBurnScAddrH        :: !ValidatorHash
    , aadaEmergencyScAddrH   :: !ValidatorHash
    , oldTokenName           :: !TokenName
    , newTokenName           :: !TokenName
    } deriving Show

contractInfo = ContractInfo
    { secretHash             = "23b2dc8595610221997288830831937455a3fe5fab6ce823a3a01252a9410d32"
    , oldAadaPolicyID        = "6f69a35a962c154bab4f2de4d9e116043697c333ade763c2b46fce6e"
    , newAadaPolicyID        = "8fef2d34078659493ce161a6c7fba4b56afefa8535296a5743f69587"
    , aadaBurnScAddrH        = "c0c671fba483641a71bb92d3a8b7c52c90bf1c01e2b83116ad7d4536"
    , aadaEmergencyScAddrH   = "a87652f3ace13f0109e01c6d035a9d2d07e1c24f9d421169cffe54a7"
    , oldTokenName           = "AADA"
    , newTokenName           = "AADA"
    }

{-# INLINABLE mkValidator #-}
mkValidator :: ContractInfo -> ExchangeDatum -> ExchangeRedeemer -> ScriptContext -> Bool
mkValidator contractInfo@ContractInfo{..} _ rdm ctx = 
    if isItEmergencyUnlock then traceIfFalse "Wrong emergency claim address" checkAadaOutEmergencyAddr
    else
     checkConditions

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    txOuts :: [TxOut]
    txOuts = txInfoOutputs info

    checkSecretHash :: TxOut -> Bool
    checkSecretHash tx = case txOutDatumHash tx of
       Just    hash -> secretHash == hash
       Nothing      -> False

    isItEmergencyUnlock :: Bool
    isItEmergencyUnlock = any checkSecretHash txOuts

    isTxToAadaEmergency :: TxOut -> Bool
    isTxToAadaEmergency tx = case toValidatorHash $ txOutAddress tx of
       Just    hash -> aadaEmergencyScAddrH == hash
       Nothing      -> False

    checkAadaOutEmergencyAddr :: Bool
    checkAadaOutEmergencyAddr = any isTxToAadaEmergency txOuts

    checkConditions :: Bool
    checkConditions = 
      traceIfFalse "Old tokens are not being sent to burn sc"                                  checkBurn   &&
      traceIfFalse "New aada amount exchange rate is bad or new aada is sent to wrong address" checkSrcDst &&
      traceIfFalse "New aada change wasn't sent back to SC"                                    checkChange &&
      traceIfFalse "Tx going back to sc datum is different"                                    checkDatum 

    isSamePolicyAada :: CurrencySymbol -> CurrencySymbol -> Bool
    isSamePolicyAada cs cs' = cs == cs'

    aadaFilter :: CurrencySymbol -> TxOut  -> Bool
    aadaFilter cs tx = any (isSamePolicyAada cs) (symbols $ txOutValue tx) 

    txOutToValueOfPolicy :: CurrencySymbol -> TokenName -> TxOut -> Integer
    txOutToValueOfPolicy cs tn tx = valueOf (txOutValue tx) cs tn 

    aadaTotalAmount :: [TxOut] -> CurrencySymbol -> TokenName -> Integer
    aadaTotalAmount txs policy tn = sum (map (txOutToValueOfPolicy policy tn) txs)

    oldAadaTotal :: Integer
    oldAadaTotal = aadaTotalAmount txOuts oldAadaPolicyID oldTokenName

    sameAmounts :: Integer -> CurrencySymbol -> TokenName -> TxOut -> Bool
    sameAmounts oldAadaAmount cs tn tx = oldAadaAmount * 1000000 == txOutToValueOfPolicy cs tn tx

    findNewAadaTx :: Maybe TxOut
    findNewAadaTx = find (sameAmounts oldAadaTotal newAadaPolicyID newTokenName) txOuts

    txGoingToBeneficiary :: TxOut -> Bool
    txGoingToBeneficiary tx = case toPubKeyHash $ txOutAddress tx of
      Just pkh -> traceIfFalse "wrong beneficiary provided in redeemer" (beneficiary rdm == pkh)
      Nothing  -> traceIfFalse "tx is not signed by receiver or aadaPkh" False

    checkSrcDst :: Bool
    checkSrcDst = case findNewAadaTx of 
       Just tx -> traceIfFalse "Found transaction but tx is not signed by receiver" (txGoingToBeneficiary tx)
       Nothing -> traceIfFalse "newAadaTx not found" False
 
    isOldAadaToBeBurnt :: TxOut -> Bool
    isOldAadaToBeBurnt tx = if aadaFilter oldAadaPolicyID tx then
          case toValidatorHash $ txOutAddress tx of
            Just    valh -> valh == aadaBurnScAddrH
            Nothing      -> traceIfFalse "Found toValidator hash which passed aada filter" False
          else
            True

    checkBurn :: Bool
    checkBurn = all isOldAadaToBeBurnt txOuts

    isTxRemainingAadaTx :: TxOut -> Bool
    isTxRemainingAadaTx tx = case findNewAadaTx of
        Just tx' -> tx /= tx'
        Nothing  -> True

    isTxGoingBack :: TxOut -> Bool
    isTxGoingBack tx = case toValidatorHash $ txOutAddress tx of
        Just valh -> valh == ownHash ctx
        Nothing   -> False

    checkChange :: Bool
    checkChange = all isTxGoingBack $ filter isTxRemainingAadaTx $ filter (aadaFilter newAadaPolicyID) txOuts

    findTxFromThisSc :: Maybe TxOut
    findTxFromThisSc = case findOwnInput ctx of
      Just txInInfo -> Just (txInInfoResolved txInInfo)
      Nothing       -> Nothing

    isDatumCorrect :: TxOut -> TxOut -> Bool
    isDatumCorrect ownTx tx = txOutDatumHash ownTx == txOutDatumHash tx

    checkDatum :: Bool
    checkDatum = case findTxFromThisSc of
      Just txo -> all (isDatumCorrect txo) $ filter isTxRemainingAadaTx $ filter (aadaFilter newAadaPolicyID) txOuts
      Nothing  -> traceIfFalse "Tx from this sc was not found" False

data Exchange
instance Scripts.ValidatorTypes Exchange where
    type instance DatumType Exchange = ExchangeDatum 
    type instance RedeemerType Exchange = ExchangeRedeemer

typedValidator :: Scripts.TypedValidator Exchange
typedValidator = Scripts.mkTypedValidator @Exchange
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode contractInfo)
     $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @ExchangeDatum @ExchangeRedeemer

validator :: Validator
validator = Scripts.validatorScript  typedValidator

PlutusTx.makeIsDataIndexed ''ContractInfo [('ContractInfo, 1)]
PlutusTx.makeLift ''ContractInfo
PlutusTx.makeIsDataIndexed ''ExchangeRedeemer [('ExchangeRedeemer, 0)]
PlutusTx.makeIsDataIndexed ''ExchangeDatum [('ExchangeDatum, 0)]

script :: Plutus.Script
script = Plutus.unValidatorScript validator

exchangeShortBs :: SBS.ShortByteString
exchangeShortBs = SBS.toShort . LBS.toStrict $ serialise script

exchange :: PlutusScript PlutusScriptV1
exchange = PlutusScriptSerialised exchangeShortBs
