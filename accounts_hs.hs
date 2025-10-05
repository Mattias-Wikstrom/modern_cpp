{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (when)
import Data.Char (toLower)
import Text.Read (readMaybe)
import Text.Printf (printf)
import System.IO (hFlush, stdout)

-- Account types as distinct data constructors
data Account
  = CheckingAccount
      { accountNumber :: Int
      , balance :: Int
      , withdrawalCount :: Int
      }
  | SavingsAccount
      { accountNumber :: Int
      , balance :: Int
      }
  deriving (Show)

-- Account creation
newChecking :: Int -> Account
newChecking n = CheckingAccount n 0 0

newSavings :: Int -> Account
newSavings n = SavingsAccount n 0

-- Format money as dollars and cents
formatMoney :: Int -> String
formatMoney cents =
  let dollars = cents `div` 100
      remainingCents = abs (cents `mod` 100)
  in printf "$%d.%02d" dollars remainingCents

-- Parse amount from string
parseAmount :: String -> Maybe Int
parseAmount s =
  let trimmed = filter (/= ' ') s
      (isNeg, numStr) = case trimmed of
        ('-':rest) -> (True, rest)
        _ -> (False, trimmed)
      parts = splitOn '.' numStr
      sign x = if isNeg then negate x else x
  in case parts of
    [dollars] -> do
      d <- readMaybe dollars
      pure $ sign (d * 100)
    [dollars, cents] -> do
      d <- readMaybe dollars
      let centsPadded = take 2 (cents ++ "00")
      c <- readMaybe centsPadded
      pure $ sign (d * 100 + c)
    _ -> Nothing
  where
    splitOn :: Char -> String -> [String]
    splitOn delim str = case break (== delim) str of
      (before, "") -> [before]
      (before, _:after) -> before : splitOn delim after

-- Deposit money
deposit :: Int -> Account -> Account
deposit amt acc = case acc of
  CheckingAccount{..} -> acc { balance = balance + amt }
  SavingsAccount{..}  -> acc { balance = balance + amt }

-- Withdrawals differ per account type
withdraw :: Int -> Account -> IO Account
withdraw amt acc = case acc of
  CheckingAccount{..}
    | balance < amt -> do
        putStrLn $ "Insufficient funds: balance " ++ formatMoney balance
        pure acc
    | otherwise -> do
        let newBalance = balance - amt
            finalBalance = if newBalance < 50000 then newBalance - 20 else newBalance
        pure acc { balance = finalBalance, withdrawalCount = withdrawalCount + 1 }

  SavingsAccount{..}
    | balance < amt -> do
        putStrLn $ "Insufficient funds: balance " ++ formatMoney balance
        pure acc
    | otherwise -> do
        putStrLn "Withdrawal from savings (fee after first withdrawal not tracked here)."
        pure acc { balance = balance - amt }

-- Display account
displayAccount :: Account -> IO ()
displayAccount acc = case acc of
  CheckingAccount{..} ->
    putStrLn $ "Checking " ++ show accountNumber ++ " = " ++ formatMoney balance
  SavingsAccount{..}  ->
    putStrLn $ "Savings " ++ show accountNumber ++ " = " ++ formatMoney balance

-- Read account number
promptAndReadAccountNo :: IO Int
promptAndReadAccountNo = do
  putStr "Enter account number: "
  hFlush stdout
  input <- getLine
  maybe (putStrLn "Invalid number, using 0" >> pure 0) pure (readMaybe input)

-- Process transactions
processAccount :: Account -> IO Account
processAccount initial = do
  putStrLn "Enter positive for deposit, negative for withdrawal, 0 to end"
  loop initial
  where
    loop acc = do
      putStr ": "
      hFlush stdout
      input <- getLine
      case parseAmount input of
        Nothing -> putStrLn "Invalid input" >> loop acc
        Just 0  -> pure acc
        Just amt
          | amt > 0 -> loop (deposit amt acc)
          | otherwise -> do
              newAcc <- withdraw (abs amt) acc
              loop newAcc

-- Bank state
data BankState = BankState
  { checkingAccounts :: [Account]
  , savingsAccounts  :: [Account]
  } deriving (Show)

emptyBankState :: BankState
emptyBankState = BankState [] []

-- Main banking loop
bankingLoop :: BankState -> IO BankState
bankingLoop st = do
  putStrLn "Enter S for Savings, C for Checking, X to exit"
  input <- getLine
  case map toLower input of
    ('c':_) -> do
      accNo <- promptAndReadAccountNo
      acc <- processAccount (newChecking accNo)
      bankingLoop st { checkingAccounts = acc : checkingAccounts st }
    ('s':_) -> do
      accNo <- promptAndReadAccountNo
      acc <- processAccount (newSavings accNo)
      bankingLoop st { savingsAccounts = acc : savingsAccounts st }
    ('x':_) -> pure st
    _       -> putStrLn "Invalid option" >> bankingLoop st

-- Totals
calculateTotal :: [Account] -> Int
calculateTotal = sum . map balance

displayTotals :: BankState -> IO ()
displayTotals BankState{..} = do
  putStrLn "\nChecking Accounts:"
  mapM_ displayAccount checkingAccounts
  putStrLn "\nSavings Accounts:"
  mapM_ displayAccount savingsAccounts
  let total = calculateTotal checkingAccounts + calculateTotal savingsAccounts
  putStrLn $ "\nTotal worth of all accounts = " ++ formatMoney total

-- Main
main :: IO ()
main = do
  final <- bankingLoop emptyBankState
  displayTotals final
