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
    , oldTokenName           :: !TokenName
    , newTokenName           :: !TokenName
    , aadaPkh                :: !PubKeyHas
    } deriving Show

contractInfo = ContractInfo
    { secretHash             = "23b2dc8595610221997288830831937455a3fe5fab6ce823a3a01252a9410d32"
    , oldAadaPolicyID        = "2b16738ac8f9cd2f0c30c8c1aef9d429ae02f5300734a902c730bb97" --CONY
    , newAadaPolicyID        = "17830caa477d5eac7a4b6c123d98fa232e6e921368b01b7cd0bdfce6" --AADA
    , aadaBurnScAddrH        = "c0c671fba483641a71bb92d3a8b7c52c90bf1c01e2b83116ad7d4536"
    , aadaEmergencyScAddrH   = "e8e5f8aa6b99363f39a7d8883652e257be3a7311c908da9ab8116bb5"
    , oldTokenName           = "CONY"
    , newTokenName           = "AADA"
    , aadaPkh                = "ff"
    }

{-# INLINABLE mkValidator #-}
mkValidator :: ContractInfo -> ExchangeDatum -> () -> ScriptContext -> Bool
mkValidator contractInfo@ContractInfo{..} _ _ ctx = 
    if isItEmergencyUnlock then traceIfFalse "Wrong emergency claim address" checkAadaOutEmergencyAddr
    else
     checkIfTxFromAada

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
    
    checkIfTxFromAada :: Bool
    checkIfTxFromAada = 
      traceIfFalse "Old tokens are not being sent to burn sc"                                  checkBurn   &&
      traceIfFalse "New aada amount exchange rate is bad or new aada is sent to wrong address" checkSrcDst &&
      traceIfFalse "New aada change wasn't sent back to SC"                                    checkChange

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

    -- change rate from 10 to 1000000 on mainnet
    sameAmounts :: Integer -> CurrencySymbol -> TokenName -> TxOut -> Bool
    sameAmounts oldAadaAmount cs tn tx = oldAadaAmount == 10 * txOutToValueOfPolicy cs tn tx 

    findNewAadaTx :: Integer -> Maybe TxOut
    findNewAadaTx i = find (sameAmounts oldAadaTotal newAadaPolicyID newTokenName) txOuts

    txSignedByReceiver :: TxOut -> Bool
    txSignedByReceiver tx = case toPubKeyHash $ txOutAddress tx of
      Just pkh -> txSignedBy info pkh
      Nothing  -> False

    checkSrcDst :: Bool
    checkSrcDst = case findNewAadaTx of 
       Just tx -> txSignedByReceiver tx || txSignedBy info aadaPkh 
       Nothing -> False

    isOldAadaToBeBurnt :: TxOut -> Bool
    isOldAadaToBeBurnt tx =
      if aadaFilter oldAadaPolicyID tx 
        then
	  case toValidatorHash $ txOutAddress tx of
            Just    valh -> valh == aadaBurnScAddrH -- || valh == ownHash ctx
            Nothing      -> False
        else
	    True

    checkBurn :: Bool
    checkBurn = all isOldAadaToBeBurnt txOuts

    checkChange :: Bool
    -- TODO
    -- the rest of first filter must be filtered yet again by txSignedByReceiver
    -- then check if all of the remaining transactions go back to this same smartcontract
    checkChange = $ filter (aadaFilter newAadaPolicyID) txOuts

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
