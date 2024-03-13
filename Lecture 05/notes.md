# Lecture 5 Native tokens

A token is defined by TokenName and CurrencySymbol, both are ByteStrings. AssetClass is a wrapper around both of them. 

To constuct them
```haskell
import Plutus.V1.Ledger.Ada
import Plutus.V1.Ledger.Value
:set -XOverloadedStrings

lovelaceValueOf 123
>> Value (Map [(,Map [("",123)])])

lovelaceValueOf 123 <> lovelaceValueOf 10
>> Value (Map [(,Map [("",133)])])

singleton "a8ff" "ABC" 7
>> Value (Map [(a8ff,Map [("ABC",7)])])

-- we can combine all, since tey are Monad
let v = singleton "a8ff" "ABC" 7 <> lovelaceValueOf 123 <> lovelaceValueOf 10
>> Value (Map [(,Map [("",133)]),(a8ff,Map [("ABC",7)])])

-- extract value
valueOf v "a8ff" "ABC"
>> 7

flattenValue v
```

The currency-symbol is the policy-id, as i prospected.

## NFT

To create nfts one can use utxo ref as condition, so it is impossible to use it more than once.


## Homework 1

Just copied the logic from vesting
```haskell
mkPolicy :: PubKeyHash -> Slot -> ScriptContext -> Bool
mkPolicy pkh deadline ctx = txSignedBy (scriptContextTxInfo ctx) pkh && to deadline `contains` txInfoValidRange (scriptContextTxInfo ctx)
```

Just copied and adapted the code from NFT.hs
```haskell
policy :: PubKeyHash -> Slot -> Scripts.MonetaryPolicy
policy pkh deadline =  mkMonetaryPolicyScript $
    $$(PlutusTx.compile [|| \pkh' deadline' -> Scripts.wrapMonetaryPolicy $ mkPolicy pkh' deadline' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh
    `PlutusTx.applyCode`
    PlutusTx.liftCode deadline

curSymbol :: PubKeyHash -> Slot -> CurrencySymbol
curSymbol pkh deadline = scriptCurrencySymbol $ policy pkh deadline
```

To test it changed the off chain code
```haskell
    if (now > deadline) && False
```

## Homework 2

too easy, just copied the code of NFT.hs and removed tn everywhere



```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
