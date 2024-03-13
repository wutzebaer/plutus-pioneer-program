# Lecture 3

## Update

first we need to update our plutus version

```bash
cd ~/workspace/plutus-pioneer-program/
sudo git pull
head -30 code/week03/cabal.project
>>> 3aa86304e9bfc425667051a8a94db73fcdc38878

cd ~/workspace/plutus
git pull
git checkout 3aa86304e9bfc425667051a8a94db73fcdc38878
nix build -f default.nix plutus.haskell.packages.plutus-core.components.library
# now drink 10-30 cups of coffee
```

Now close your VSCode, and both playground shells (backend+frontend)
```bash
# start backend
cd ~/workspace/plutus
nix-shell
cd plutus-playground-client
plutus-playground-server

# start frontend
cd ~/workspace/plutus
nix-shell
cd plutus-playground-client
npm run start
```
While building the dev container i now got an error

```bash
error: a 'x86_64-linux' with features {kvm} is required to build '/nix/store/7gdiih04v7b9zwzgkfwjfgrmfvd2c8bg-docker-layer-plutus-devcontainer.drv', but I am a 'x86_64-linux' with features {benchmark, big-parallel, nixos-test}
```

so in chat the people told to just set it and it seems to work

```bash
# fake kvm
sudo mkdir /etc/nix
sudo vi /etc/nix/nix.conf
system-features = kvm

# rebuild dev container
cd ~/workspace/plutus
sudo docker load < $(nix-build default.nix -A devcontainer)

# But the dev container wont start up on the new version, so i had to do this: https://cardano.stackexchange.com/questions/382/problem-starting-a-plutus-devcontainer-in-windows

# restart VSCode
cd ~/workspace/plutus-pioneer-program/
code .
```

now that the container does not run as root anymore we have to give permissions for the mounted dir (run in your wsl)

```bash
sudo chown 1000 ~/.cabal/packages/
```

Click reopen in container and open console in vscode and check if everything works
```bash
cd /workspaces/plutus-pioneer-program/code/week03/
cabal update
cabal build
```

## further problems

after some days i wanted to resume this lecutre and got errors. So i had to:

  - netstat and kill processes on port 8080
  - netstat and kill processes on port 8009
  - still couldnt get the dev container run so i reset my wsl and started over from scratch
  - 


## Needed Codechanges for last week

Since plutus is updated some things must be changed in our last code, to make it compatible to the new version.

The updated version is already in the repo, here a diff

![Diff](https://peterspace.de/week3diff.png)

## Changes in Playground

   * we dont need to remove the imports
   * fees are simulated

## Context

Datum and redeemer can be custom types as long as they implement IsData, but the context is scriptContext (was validatorContext in the old commit). The examples so far looked only at the datum and redeemer.

In this lecure we look at the context: https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Contexts.hs

```haskell
data ScriptContext = ScriptContext{scriptContextTxInfo :: TxInfo, scriptContextPurpose :: ScriptPurpose }

data TxInfo = TxInfo
    { txInfoInputs      :: [TxInInfo] -- ^ Transaction inputs
    , txInfoOutputs     :: [TxOut] -- ^ Transaction outputs
    , txInfoFee         :: Value -- ^ The fee paid by this transaction.
    , txInfoForge       :: Value -- ^ The 'Value' forged by this transaction.
    , txInfoDCert       :: [DCert] -- ^ Digests of certificates included in this transaction
    , txInfoWdrl        :: [(StakingCredential, Integer)] -- ^ Withdrawals
    , txInfoValidRange  :: SlotRange -- ^ The valid range for the transaction.
    , txInfoSignatories :: [PubKeyHash] -- ^ Signatures provided with the transaction, attested that they all signed the tx
    , txInfoData        :: [(DatumHash, Datum)]
    , txInfoId          :: TxId
    -- ^ Hash of the pending transaction (excluding witnesses)
    } deriving (Generic)

data ScriptPurpose
    = Minting CurrencySymbol
    | Spending TxOutRef
    | Rewarding StakingCredential
    | Certifying DCert
```

There is a lot of informatio nbut in this lecture we concentrate on `txInfoValidRange`

https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Slot.hs


```haskell
# Slot is just an integer wrapper
newtype Slot = Slot { getSlot :: Integer }
    deriving stock (Haskell.Eq, Haskell.Ord, Show, Generic)
    deriving anyclass (FromJSON, FromJSONKey, ToJSON, ToJSONKey, NFData)
    deriving newtype (Haskell.Num, AdditiveSemigroup, AdditiveMonoid, AdditiveGroup, Enum, Eq, Ord, Real, Integral, Serialise, Hashable, PlutusTx.IsData)

type SlotRange = Interval Slot
```

https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Interval.hs

```haskell
# the most common constructor takey lower and upper bound
interval :: a -> a -> Interval a
# from till any time
from :: a -> Interval a
# just valid until then
to :: a -> Interval a
```
Lets play in repl

```haskell
cabal repl
import Plutus.V1.Ledger.Slot
import Plutus.V1.Ledger.Interval

Slot 3
>> Slot {getSlot = 3}

3 :: Slot
>> Slot {getSlot = 3}

interval (Slot 3) 10
>> Interval {ivFrom = LowerBound (Finite (Slot {getSlot = 3})) True, ivTo = UpperBound (Finite (Slot {getSlot = 10})) True}

member 5 $ interval (Slot 3) 10
>> True

member 10 $ interval (Slot 3) 10
>> True

member 11 $ interval (Slot 3) 10
>> False

from (Slot 20)
>> Interval {ivFrom = LowerBound (Finite (Slot {getSlot = 20})) True, ivTo = UpperBound PosInf True}

member 11 $ from (Slot 12)
>> False

to (Slot 20)
>> Interval {ivFrom = LowerBound NegInf True, ivTo = UpperBound (Finite (Slot {getSlot = 20})) True}

contains (to $ Slot 100) $ interval 10 110
>> False

overlaps (to $ Slot 100) $ interval 10 110
>> True
```

## Vesting.hs

Only a certain person can access the money, but only after a certain time.

```haskell
# remember the datum is set by the creator of the script address
data VestingDatum = VestingDatum
    { beneficiary :: PubKeyHash
    , deadline    :: Slot
    } deriving Show
```

```haskell
# datum is vesting datum and redeemer is not important, since we get the redeemer addrress from the context
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool
```

More interesing is now the validator function itself

```haskell
mkValidator dat () ctx =
    traceIfFalse "beneficiary's signature missing" checkSig      &&
    traceIfFalse "deadline not reached"            checkDeadline
  where
    info :: TxInfo
#   helper shortcut function to ctx.scriptContextTxInfo (wired syntax of haskell)
    info = scriptContextTxInfo ctx

    checkSig :: Bool
#   check if dat.beneficiary is element of info.txInfoSignatories (again wired syntax, they like to reverse it in haskell)
#   `elem` is a helper function from prelude which checks if a list contains something
    checkSig = beneficiary dat `elem` txInfoSignatories info

#   check if the valid range of the transaction is completly after the deadline
    checkDeadline :: Bool
    checkDeadline = from (deadline dat) `contains` txInfoValidRange info
```

## Playground

since we have to input the address of our redeemer we have to find it with repl

```haskell
import Wallet.Emulator
import Ledger

:t walletPubKey
# returns the pubKey of a wallet
>> walletPubKey :: Wallet -> PubKey

:t pubKeyHash
# takes a pubKey and converts it into hash
>> pubKeyHash :: PubKey -> PubKeyHash

:i Wallet
# It is just a wrapper around integer
>> type Wallet :: *
>> newtype Wallet = Wallet {getWallet :: Integer}
>>         -- Defined in `Wallet.Emulator.Wallet'
>> instance Eq Wallet -- Defined in `Wallet.Emulator.Wallet'
>> instance Ord Wallet -- Defined in `Wallet.Emulator.Wallet'
>> instance Show Wallet -- Defined in `Wallet.Emulator.Wallet'

# take wallet 2, get pub key, convert it into hash (read backwards again)
pubKeyHash $ walletPubKey $ Wallet 2
39f713d0a644253f04529421b9f51b9b08979d08295959c4f3990ee617f5139f
```

## Parameterized crontacts

When multiple users give and grab it's complicated to prospect who gets which output. With parameterized both get their own script address which is more convenient.

So we make mkValidator a function which takes a param and then returns the actual valicdate function, which uses this param.

So we dont use out VestingParam as Datum, but as param four our function which generates the validator-function.

Since the plutus compiler runs at compile time he can't handle this flawless. Therefore we add this apply function part. It assumes the first one is a function and the second one a parameter. Since apply code also needs a plutus script we use lifeCode. 

```haskell
( *** `PlutusTx.applyCode` PlutusTx.liftCode p)
```

# Homework

## Part1

The first part was without parameterized. First i copied the stuff from vesting.hs and adapted it a bit. Since Haskell has a ternary operator https://en.wikipedia.org/wiki/%3F:#Haskell it was easy, just added a checkSig2 and used the operator.

```haskell
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkValidator dat () ctx = if checkDeadline then checkSig2 else checkSig1
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    checkSig1 :: Bool
    checkSig1 = beneficiary1 dat `elem` txInfoSignatories info

    checkSig2 :: Bool
    checkSig2 = beneficiary2 dat `elem` txInfoSignatories info

    checkDeadline :: Bool
    checkDeadline = from (deadline dat) `contains` txInfoValidRange info
```
## Part2

For part 2 i copied the validator body from vesting.hs too but used the direct parameters
```haskell
mkValidator :: PubKeyHash -> Slot -> () -> ScriptContext -> Bool
mkValidator beneficiary deadline () ctx =
    traceIfFalse "beneficiary's signature missing" checkSig      &&
    traceIfFalse "deadline not reached"            checkDeadline
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    checkSig :: Bool
    checkSig = beneficiary `elem` txInfoSignatories info

    checkDeadline :: Bool
    checkDeadline = from deadline `contains` txInfoValidRange info
```

For inst we can also copy the code from Parameterized.hs. But we have to change the identity to slot on the wrap function. 
```haskell
inst :: PubKeyHash -> Scripts.ScriptInstance Vesting
inst p = Scripts.validator @Vesting
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Slot @()
```

the remaining boilerplate can also be copied 1:1
```haskell
validator :: PubKeyHash -> Validator
validator = Scripts.validatorScript . inst

scrAddress :: PubKeyHash -> Ledger.Address
scrAddress = scriptAddress . validator
```
this is because the `data Vesting` has now Slot as DatumType. Because Slot is the Datum now.
```haskell
data Vesting
instance Scripts.ScriptType Vesting where
    type instance DatumType Vesting = Slot
    type instance RedeemerType Vesting = ()
```



