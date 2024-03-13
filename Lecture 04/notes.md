# Lecture 4

The plutus' pelude has added these pragams for all functions so we can easily use them in our validator functions. But There are million of other libraries which haven't plutus in mind and of course lack of these pragmas. Therefor the code will be simple and won't have many dependencies. 

In offchain all is just haskell. But needs much more haskell magic and monads https://tutswiki.com/yet-another-lousy-monad-tutorial/ 

Functions in haskell have a feature called `referencial transparency` which means a function returns always the same value if the parameters are the same. Unlinke in java it could read from a console/db etc and always return another value.

But even in haskell we need input otherwise the program would do nothing interesting. To deal with side effects haskell has a so called `io-monad`.

```haskell
# io is the type constructor which takes here int as a param
foo :: IO Int
foo = ....
```

It means that this function is a computation which results in an int. It is only callable in the main routine

For example getLine and Readline are also of type IO, along with read/write files/sockets etc.

More compley example where the operator `>>=` binds (waits for) the result of the first IO (recipe) to the next
```haskell
bar :: IO ()

      --getLine and bind the result to lamda s
bar = getLine >>= \s ->
      --getLine and bind the result to lamda t
      getLine >>= \t ->
      --print concatinated
      putStrLn (s++t)
```

Also maybe is an Monad, it's like optional. 

## PLutus monats

Contract monat defines code that will run in the wallet. EmulatorTrace monat is someting like the playground.

# Homework
```haskell
payTrace x y = do
    --Start contract with wallet1
    w1 <- activateContractWallet (Wallet 1) payContract
    --Call enbdpoint and create PayParams object while retrieving pubkeyhash of wallet 2, with the x amount
    callEndpoint @"pay" w1 $ PayParams {
        ppRecipient = pubKeyHash $ walletPubKey $ Wallet 2,
        ppLovelace = x
    }
    void $ Emulator.waitNSlots 1
    --Call enbdpoint and create PayParams object while retrieving pubkeyhash of wallet 2, with the y amount
    callEndpoint @"pay" w1 $ PayParams {
        ppRecipient = pubKeyHash $ walletPubKey $ Wallet 2,
        ppLovelace = y
    }
    void $ Emulator.waitNSlots 1
```

