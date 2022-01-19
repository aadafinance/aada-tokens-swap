# aada-tokens-exchange

Cardano smartcontract code for aada finance tokens exchange

## Burn SC

Contract does no validation and always fails. This secures that old aada tokens may never be unlocked.

## Exchange SC

New aada tokens are to be locked in this smartcontract for exchange with old aada tokens.
Aada tokens can be exchanged only on these conditions:
- rate of oldAADA tokens to newAADA tokens is 1 to 1000000
- oldAADA tokens are sent to burnSC this way ensuring no one can use oldAADA tokens anymore after exchange
- transaction must be signed by aada finance backend or the beneficiary to whom newAADA tokens are to be sent
- remaining newAADA tokens from unlocking tx must be sent back to this same smartcontract
