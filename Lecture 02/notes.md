# Lecture 2

## Pieces of data

  * Datum (utxo)
  * Redeemer (input under validation)
  * Context (Transaction currently validated, with inputs and outputs)

from https://docs.cardano.org/projects/plutus/en/latest/plutus/tutorials/basic-validators.html

Validators receive some information from the validating node:

  * The datum, which is some script-specific data specified by the party who created the output.
  * The redeemer, which is some script-specific data specified by the party spending the output.
  * The validation context, which contains a representation of the spending transaction, as well as the index of the input whose validator is currently being run.


All datatypes use the same lowlevel datatype in haskell: Data from PlutusTX.Data

https://github.com/input-output-hk/plutus/blob/master/plutus-tx/src/PlutusTx/Data.hs

```haskell
-- | A generic "data" type.
--
-- The main constructor 'Constr' represents a datatype value in sum-of-products
-- form: @Constr i args@ represents a use of the @i@th constructor along with its arguments.
--
-- The other constructors are various primitives.
data Data =
    Constr Integer [Data]
    | Map [(Data, Data)]
    | List [Data]
    | I Integer
    | B BS.ByteString
```

## Explore in repl

With your VScode from Lecture 1 goto the week2 folder and start repl. **To exit hit ctrl+d**

    cd code/week02/
    cabal repl

And do some commands

```haskell
import PlutusTx
:i Data --inspect the Data type
I 7     --create integer of 7
:t I 7  --lookup type of integer of 7
``` 
To use the constructor for bytestrings, we need to create bytestrings. We use the OverloadedStrings extension.

```haskell
:t B "Haskell" --gives an error without the extension

:set -XOverloadedStrings --now it will work
:t B "Haskell" --woks only if we imported PlutusTx before
```

try the map
```haskell
Map [(I 7, B "Haskell"), (List [I 0], I 1000)]
:t Map [(I 7, B "Haskell"), (List [I 0], I 1000)]
```

## Create validator (Gift.hs)

If you are not familiar with haskell, take a quick lecture about the arrow operator "->":

https://www.tutorialspoint.com/haskell/haskell_functions.htm

**What i understand**: The first Types are the input parameters and the last is always the return type.

```haskell
-- declare the function mkValidator
-- Parameters: Datum, Redeemer, Context
-- Result: () Unit aka void
mkValidator :: Data -> Data -> Data -> ()
```

check  out unit in repl
```haskell
:l src/Week02/Gift.hs
()
```

Functions in haskell without IO are not allowed to have side effects, so only the return value is considered. Beside returning somethinf the function can throw an exception, that is what plutus uses if a validation fails.

Now implement mkValidator

To better understand the underscores: https://andrew.gibiansky.com/blog/haskell/haskell-syntax/#_pattern_matching_and_branching

```haskell
-- the underscores _ mean that the three arguments are ignored, it might have something to do with overloading for different types later
mkValidator _ _ _ = ()
```

Recheck in repl console to find syntax errors
```haskell
:r
Ok, one module loaded.
```

In order to turn mkValidator into a plutus script we need to convert it into a Validator. This part is boilderplate and we dont need to understand it now. It's some kind of pre compiler. It's called TemplateHaskell https://wiki.haskell.org/Template_Haskell
```haskell
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])
```

the function mkValidatorScript expects Data -> Data -> Data -> ()
```haskell
mkValidatorScript :: CompiledCode (Data -> Data -> Data -> ()) -> Validator
```

in order to reference our mkValidator inside the oxford-brackets the declaration has to be commentd with the inlineable pragma. Otherwise we had to put the code itself into the oxford-brackets. All further used functions also have to be inlineable.
```haskell
{-# INLINABLE mkValidator #-}
```

Recheck in repl
```haskell
:r
Ok, one module loaded.
validator
Validator { <script> }
:t validator
validator
  :: plutus-ledger-api-0.1.0.0:Plutus.V1.Ledger.Scripts.Validator
```

Create the hash of our validator function
```haskell
valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator
```
Turn it into an actual address (Plutus-Script-Address)
```haskell
scrAddress :: Ledger.Address
scrAddress = ScriptAddress valHash
```
To test it in the playground just copy and paste the whole Gift.hs into playground. Before compile in playground remove the Language pragma block at the beginning.

The off-chain part below is not in the focus of this lecture.

## Let the validation fail (Burn.hs)

Change the implementation to throw an error rather than return unit
```haskell
mkValidator _ _ _ = error () --no error message, the error function from the default prelude takes a string, but this is the version from plutus
mkValidator _ _ _ = traceError "NO WAY!" --with helpful error message
```
Check in repl
```haskell
:l src/Week02/Burn.hs 
Ok, one module loaded.
:t error
error :: [Char] -> a
:t PlutusTx.Prelude.error
PlutusTx.Prelude.error :: () -> a
```

The Prelude package from Haskell is in Plutus es exchanged with a custom version where all the functions are marked with the inlineable praga. In order to this the default Prelude is disabled with `{-# LANGUAGE NoImplicitPrelude   #-}` and `PlutusTx.Prelude` is imported.

In the example the Semigroup from the default Prelude is imported, but that doesn't matter now.

**traceError** takes not a normal haskell string, but a plutus string. To make this work the pragma `{-# LANGUAGE OverloadedStrings   #-}` is included on the top, which converts the literals automatically.

## Write a validator which acutally validates something (FortyTwo.hs)

Now care about redeemer

Remember: `The redeemer, which is some script-specific data specified by the party spending the output.`

```haskell
mkValidator _ r _
    | r == I 42 = () --if redeemer is 42 it is ok
    | otherwise = traceError "wrong redeemer" --else throw an exception
```

Now we check if r is integer 42, but since the declaration is still with Data we also could pass any other type like Float or String without errors. Only the Frontend input code requires an integer.

## Now make the Validator typed (Typed.hs)

Before we just read Data Data Data which is lowlevel and generic, now we use specific types.

```haskell
-- Datum should be unit (), means nothing
-- Redeemer should be an integer
-- Context should be a ValidatorCtx
-- Finally we will return a bool instead of unit
mkValidator :: () -> Integer -> ValidatorCtx -> Bool
```
Types vor Context=ValidatorCtx and Returntype=bool will always be the same. Only the Datum and Redeemer will change wihle you write your scripts.

The implementation is also changed:
```haskell
--unit for Datum, varaible named r (now an integer) as Redeemer, still ignore Context
mkValidator () r _ 
    -- since we know r is an integer we dent need to check with I, and now we return true
    | r == 42   = True 
    --return false if wrong number is passed
    | otherwise = False 
```
The part `$$(PlutusTx.compile [|| mkValidator ||])` is not more type correct, for that we need to import `Ledger.Typed.Scripts` and this boilerplate code
```haskell
data Typed
instance Scripts.ScriptType Typed where
    type instance DatumType Typed = () --adapt
    type instance RedeemerType Typed = Integer --adapt

inst :: Scripts.ScriptInstance Typed
inst = Scripts.validator @Typed
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @Integer --adapt

validator :: Validator
validator = Scripts.validatorScript inst
```
When we change our input types later we have to adapt the parths with () and Integer.

The client code has also changed a bit, but it is still not in focus.

## How does the conversion from out types to the lowlevel Data type work

https://github.com/input-output-hk/plutus/blob/master/plutus-tx/src/PlutusTx/IsData/Class.hs
```haskell
class IsData (a :: Type) where
    toData :: a -> Data
    -- TODO: this should probably provide some kind of diagnostics
    fromData :: Data -> Maybe a
```

The class has the functions toData and fromData. Try it in REPL
```haskell
:l src/Week02/Typed.hs
import PlutusTx.IsData
:i IsData

toData (42 :: Integer) --will use the I constructor

import PlutusTx
fromData (I 42) :: Maybe Integer
fromData (List []) :: Maybe Integer --cannot be converted, therefore we get Nothing
```

IsData currently supporty only a few types and will be extended later. If we need to use custom typed in our contract we habe to provide implementations for it.

first we make our validation function a oneliner
```haskell
--change
mkValidator () r _
    | r == 42   = True
    | otherwise = False
--to
mkValidator () r _ = r == 42
```

to still get an helpful error message without throw error we change it to
```haskell
mkValidator () r _ = traceIfFalse "wrong redeemer" $ r == 42
```

Now we try to use a custom type for redeemer and therefore create a wrapper type for integer
```haskell
newtype MySillyRedeemer = MySillyRedeemer Integer
    deriving Show --derive show function, seems to be something like String

mkValidator :: () -> MySillyRedeemer -> ValidatorCtx -> Bool
mkValidator () (MySillyRedeemer r) _ = traceIfFalse "wrong redeemer" $ r == 42
```

and also need to adapt the boilerplate code by exchaning the Integer with MySillyRedeemer
```haskell
inst :: Scripts.ScriptInstance Typed
inst = Scripts.validator @Typed
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @MySillyRedeemer
```
Now we will stil get an error, because IsData does not know how to converti it into Data. So we use another bit of of Template Hskell to auto generate it:
```haskell
PlutusTx.unstableMakeIsData ''MySillyRedeemer
```

To test it in REPL
```haskell
import PlutusTx.IsData
toData (MySillyRedeemer 17)
>>> Constr 0 [I 17]
import PlutusTx
fromData (Constr 0 [I 3]) :: Maybe MySillyRedeemer
>>> Just (MySillyRedeemer 3)
```

To make it work in simulator we also need to change the frontend code
```haskell
--from
tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ I r | oref <- orefs]
--to
tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData $ MySillyRedeemer r | oref <- orefs]
```

# Homework 1

## What i tried
```haskell
mkValidator () (a,b) _ = traceIfFalse "wrong redeemer" $ a!=b --wrong syntax
mkValidator () (a,b) _ = traceIfFalse "wrong redeemer" $ not(a == b) --found this at https://en.wikibooks.org/wiki/Haskell/Truth_values
mkValidator () (a,b) _ = traceIfFalse "wrong redeemer" $ a /= b --VSCode Sugested me /=, seems that it is the equivalent for !=
--looks good now
```

The next one was easy, just replace Integer with (Bool, Bool)
```haskell
data Typed
instance Scripts.ScriptType Typed where
    type instance DatumType Typed = () --adapt
    type instance RedeemerType Typed = Integer --adapt

inst :: Scripts.ScriptInstance Typed
inst = Scripts.validator @Typed
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @Integer --adapt

validator :: Validator
validator = Scripts.validatorScript inst

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

scrAddress :: Ledger.Address
scrAddress = ScriptAddress valHash
```

Tried in REPL
```haskell
:l src/Week02/Homework1.hs
>>> Ok, one module loaded. -- seems to be correct
```

Also tried the code in Playground, now i had to enter two booleans

But got an wrong behaviour: It seems i twisted the condition, because it was just ok if the booleans are different, so i changed it to traceIfTrue
```haskell
mkValidator () (a,b) _ = traceIfTrue "wrong redeemer" $ a /= b
```

but it is still wired, if i set both to true or both to false i get an error without message
```haskell
Validation failed: 77143a89ee71512d9dc918d32982e24794a9f18c4d5d841322a77ce89f9ad928
 (ScriptFailure (EvaluationError []))
```
if i set them different for exmaple false and true everything runs and he gets the money although it should not

even with the not() version i get the same wired behaviour
```haskell
mkValidator () (a,b) _ = traceIfTrue "wrong redeemer" $ not(a == b)
```

after that i tried
```haskell
mkValidator () (a,b) _ = traceIfFalse "wrong redeemer" $ a == b
```
which worked, it seems that traceIfTrue is screwed. But dino from the help chat pointed me to my error:
```haskell
traceIfTrue "wrong redeemer" $ a /= b
--when a==b the result of this expression is false, it won't throw an exception but since the result is false the validation still fails, we always need an true in the end to make the transaction work

--for the other case we look at the implementation
traceIfTrue :: Builtins.String -> Bool -> Bool
traceIfTrue str a = if a then trace str True else False
--it seems it will never throw an exception, it just traces the message and passes the True further
--so i still would expect the trace message in the logs, but maybe they are only printed if the transaction fails
```

## Homework 2

Seems very similar, but what was a little nut as java programmer is that the property comes first(flag1) and than the object(myr):
```haskell
mkValidator () myr _ = traceIfFalse "wrong redeemer" $ flag1 myr == flag2 myr
```

and then again the adapted boilerplate
```haskell
data Typed
instance Scripts.ScriptType Typed where
    type instance DatumType Typed = () --adapt
    type instance RedeemerType Typed = MyRedeemer --adapt

inst :: Scripts.ScriptInstance Typed
inst = Scripts.validator @Typed
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @MyRedeemer --adapt

validator :: Validator
validator = Scripts.validatorScript inst

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

scrAddress :: Ledger.Address
scrAddress = ScriptAddress valHash
```

That's it