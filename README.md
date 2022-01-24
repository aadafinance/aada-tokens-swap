# aada-tokens-exchange

Cardano smartcontract code for aada finance tokens exchange

## Burn SC

Contract does no validation and always fails. This secures that old aada tokens may never be unlocked.

## Exchange SC

New aada tokens are to be locked in this smartcontract for exchange with old aada tokens.
Aada tokens can be exchanged only on these conditions:
- rate of oldAADA tokens to newAADA tokens is 1 to 1000000
- oldAADA tokens are sent to burnSC this way ensuring no one can use oldAADA tokens anymore after exchange
- newAAADA tokens must be sent to address provided by redeemer
- remaining newAADA tokens from unlocking tx must be sent back to this same smartcontract
