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

-- This example is taken directly from cardano-api, written by Jordan Millar, IOHK

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
import           Plutus.V1.Ledger.Time 
import           Plutus.V1.Ledger.Scripts
import           Plutus.V1.Ledger.Credential
import           Plutus.V1.Ledger.Api
import           Plutus.V1.Ledger.Value
import qualified PlutusTx
import           PlutusTx.Prelude hiding (Semigroup (..), unless)
import           Prelude              (IO, Semigroup (..), Show (..), String)

import           Ledger.Typed.Scripts as Scripts


data ContractInfo = ContractInfo
    { secretHash             :: !DatumHash
    , oldAadaPolicyID        :: !CurrencySymbol	
    , newAadaPolicyID        :: !CurrencySymbol
    , aadaBurnScAddrH        :: !ValidatorHash
    , aadaEmergencyScAddrH   :: !ValidatorHash
    , tokenName              :: !TokenName
    } deriving Show

contractInfo = ContractInfo
    { secretHash             = "23b2dc8595610221997288830831937455a3fe5fab6ce823a3a01252a9410d32"
    , oldAadaPolicyID        = "ff"
    , newAadaPolicyID        = "ff"
    , aadaBurnScAddrH        = "e8e5f8aa6b99363f39a7d8883652e257be3a7311c908da9ab8116bb5"
    , aadaEmergencyScAddrH   = "e8e5f8aa6b99363f39a7d8883652e257be3a7311c908da9ab8116bb5"
    , tokenName              = "AADA"
    }

{-# INLINABLE mkValidator #-}
mkValidator :: ContractInfo -> () -> () -> ScriptContext -> Bool
mkValidator contractInfo@ContractInfo{..} _ _ ctx = 
    if isItEmergencyUnlock then traceIfFalse "Wrong emergency claim address" checkAadaOutEmergencyAddr
    else
     traceIfFalse "Funds are not being unlocked to address owner funds were locked from"  checkSrcDst  &&
     traceIfFalse "Invalid tokens exchange amount"                                        checkAmounts &&
     traceIfFalse "Old tokens are not being sent to burn sc"                              checkBurn
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

    isSamePolicyAada :: CurrencySymbol -> CurrencySymbol -> Bool
    isSamePolicyAada cs cs' = cs == cs'

    aadaFilter :: CurrencySymbol -> TxOut  -> Bool
    aadaFilter cs tx = any (isSamePolicyAada cs) (symbols $ txOutValue tx) 

    txOutToValueOfPolicy :: CurrencySymbol -> TxOut -> Integer
    txOutToValueOfPolicy cs tx = valueOf (txOutValue tx) cs tokenName

    aadaTotalAmount :: [TxOut] -> CurrencySymbol -> Integer
    aadaTotalAmount txs policy = sum (map (txOutToValueOfPolicy policy) txs)

    oldAada :: Integer
    oldAada = aadaTotalAmount txOuts oldAadaPolicyID

    newAada :: Integer
    newAada = aadaTotalAmount txOuts newAadaPolicyID

    checkAmounts :: Bool
    checkAmounts = oldAada == newAada * 1000000    

    signedByPkh :: TxOut -> Bool
    signedByPkh tx = case toPubKeyHash $ txOutAddress tx of
      Just    pkh -> txSignedBy info pkh
      Nothing     -> False

    checkSrcDst :: Bool
    checkSrcDst = all signedByPkh (filter (aadaFilter newAadaPolicyID) txOuts)

    isOldAadaToBeBurnt :: TxOut -> Bool
    isOldAadaToBeBurnt tx =
      if aadaFilter oldAadaPolicyID tx 
        then
	  case toValidatorHash $ txOutAddress tx of
            Just    valh -> valh == aadaBurnScAddrH || valh == ownHash
            Nothing      -> False
        else
	    True

    checkBurn :: Bool
    checkBurn = all isOldAadaToBeBurnt txOuts

{-
data Exchange
instance Scripts.ValidatorTypes Exchange where
    type instance DatumType Exchange = ExchangeDatum
    type instance RedeemerType Exchange = ExchangeRedeemer
-}

data Exchange
instance Scripts.ValidatorTypes Exchange

typedValidator :: Scripts.TypedValidator Exchange
typedValidator = Scripts.mkTypedValidator @Exchange
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode contractInfo)
     $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator

validator :: Validator
validator = Scripts.validatorScript  typedValidator

PlutusTx.makeIsDataIndexed ''ContractInfo [('ContractInfo, 1)]
PlutusTx.makeLift ''ContractInfo
--PlutusTx.makeIsDataIndexed ''ExchangeDatum [('ExchangeDatum, 0)]
--PlutusTx.makeIsDataIndexed ''ExchangeRedeemer [('ExchangeRedeemer, 0)]

script :: Plutus.Script
script = Plutus.unValidatorScript validator

exchangeShortBs :: SBS.ShortByteString
exchangeShortBs = SBS.toShort . LBS.toStrict $ serialise script

exchange :: PlutusScript PlutusScriptV1
exchange = PlutusScriptSerialised exchangeShortBs
