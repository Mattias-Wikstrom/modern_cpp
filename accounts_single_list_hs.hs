{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (when)
import Data.Char (toLower)
import Text.Read (readMaybe)
import Text.Printf (printf)
import System.IO (hFlush, stdout)

-- =====================================================
-- Money utilities
-- =====================================================

formatMoney :: Int -> String
formatMoney cents =
  let dollars = cents `div` 100
      remainingCents = abs (cents `mod` 100)
  in printf "$%d.%02d" dollars remainingCents

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

promptAndReadAccountNo :: IO Int
promptAndReadAccountNo = do
  putStr "Enter account number: "
  hFlush stdout
  input <- getLine
  maybe (putStrLn "Invalid number, using 0" >> pure 0) pure (readMaybe input)

-- =====================================================
-- Account typeclass
-- =====================================================

class AccountLike a where
  accountNo     :: a -> Int
  acntBalance   :: a -> Int
  deposit       :: Int -> a -> a
  withdrawal    :: Int -> a -> IO a
  display       :: a -> IO ()

processAccount :: AccountLike a => a -> IO a
processAccount acc = do
  putStrLn "Enter positive for deposit, negative for withdrawal, 0 to end"
  loop acc
  where
    loop a = do
      putStr ": "
      hFlush stdout
      input <- getLine
      case parseAmount input of
        Nothing -> putStrLn "Invalid input" >> loop a
        Just 0  -> pure a
        Just amt
          | amt > 0 -> loop (deposit amt a)
          | otherwise -> do
              a' <- withdrawal (abs amt) a
              loop a'

-- =====================================================
-- Common account data
-- =====================================================

data AccountData = AccountData
  { accNumber  :: Int
  , accBalance :: Int
  } deriving (Show)

-- =====================================================
-- Checking Account
-- =====================================================

data Checking = Checking
  { chkData        :: AccountData
  , chkWithdrawals :: Int
  } deriving (Show)

newChecking :: Int -> Checking
newChecking n = Checking (AccountData n 0) 0

instance AccountLike Checking where
  accountNo = accNumber . chkData
  acntBalance = accBalance . chkData

  deposit amt acc =
    acc { chkData = (chkData acc) { accBalance = accBalance (chkData acc) + amt } }

  withdrawal amt acc
    | bal < amt = do
        putStrLn $ "Insufficient funds: balance " ++ formatMoney bal
        pure acc
    | otherwise = do
        let newBal = bal - amt
            finalBal = if newBal < 50000 then newBal - 20 else newBal
        pure acc
          { chkData = (chkData acc) { accBalance = finalBal }
          , chkWithdrawals = chkWithdrawals acc + 1
          }
    where
      bal = accBalance (chkData acc)

  display acc =
    putStrLn $ "Checking " ++ show (accountNo acc)
            ++ " = " ++ formatMoney (acntBalance acc)

-- =====================================================
-- Savings Account
-- =====================================================

data Savings = Savings
  { savData        :: AccountData
  , savWithdrawals :: Int
  } deriving (Show)

newSavings :: Int -> Savings
newSavings n = Savings (AccountData n 0) 0

instance AccountLike Savings where
  accountNo = accNumber . savData
  acntBalance = accBalance . savData

  deposit amt acc =
    acc { savData = (savData acc) { accBalance = accBalance (savData acc) + amt } }

  withdrawal amt acc
    | bal < amt = do
        putStrLn $ "Insufficient funds: balance " ++ formatMoney bal
        pure acc
    | otherwise = do
        let fee = if savWithdrawals acc >= 1 then 500 else 0
        pure acc
          { savData = (savData acc)
              { accBalance = bal - amt - fee }
          , savWithdrawals = savWithdrawals acc + 1
          }
    where
      bal = accBalance (savData acc)

  display acc =
    putStrLn $ "Savings " ++ show (accountNo acc)
            ++ " = " ++ formatMoney (acntBalance acc)

-- =====================================================
-- Existential wrapper for mixed accounts
-- =====================================================

data AnyAccount where
  AnyAccount :: AccountLike a => a -> AnyAccount

instance AccountLike AnyAccount where
  accountNo (AnyAccount a) = accountNo a
  acntBalance (AnyAccount a) = acntBalance a
  deposit amt (AnyAccount a) = AnyAccount (deposit amt a)
  withdrawal amt (AnyAccount a) = do
    a' <- withdrawal amt a
    pure (AnyAccount a')
  display (AnyAccount a) = display a

-- Optional Show instance for debugging
instance Show AnyAccount where
  show (AnyAccount a) = "AnyAccount #" ++ show (accountNo a)
                     ++ " Balance: " ++ formatMoney (acntBalance a)

-- =====================================================
-- Bank state
-- =====================================================

data BankState = BankState
  { allAccounts :: [AnyAccount]
  } -- no deriving Show needed

emptyBankState :: BankState
emptyBankState = BankState []

-- =====================================================
-- Banking loop
-- =====================================================

bankingLoop :: BankState -> IO BankState
bankingLoop st = do
  putStrLn "Enter S for Savings, C for Checking, X to exit"
  input <- getLine
  case map toLower input of
    ('c':_) -> do
      accNo <- promptAndReadAccountNo
      acc <- processAccount (newChecking accNo)
      bankingLoop st { allAccounts = AnyAccount acc : allAccounts st }

    ('s':_) -> do
      accNo <- promptAndReadAccountNo
      acc <- processAccount (newSavings accNo)
      bankingLoop st { allAccounts = AnyAccount acc : allAccounts st }

    ('x':_) -> pure st
    _       -> putStrLn "Invalid option" >> bankingLoop st

-- =====================================================
-- Display totals
-- =====================================================

displayTotals :: BankState -> IO ()
displayTotals BankState{..} = do
  putStrLn "\nAll Accounts:"
  mapM_ display allAccounts
  let total = sum (map acntBalance allAccounts)
  putStrLn $ "\nTotal worth of all accounts = " ++ formatMoney total

-- =====================================================
-- Main
-- =====================================================

main :: IO ()
main = do
  final <- bankingLoop emptyBankState
  displayTotals final

