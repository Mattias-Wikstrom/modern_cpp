{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

-- Bank Account System in Haskell; Conversion by Claude.ai
module Main where

import Control.Monad (when)
import Data.Char (toLower)
import Text.Read (readMaybe)
import Text.Printf (printf)
import System.IO (hFlush, stdout)

-- Account types
data AccountType = Checking | Savings
  deriving (Show, Eq)

-- Account data structure (immutable)
data Account = Account
  { accountNumber :: Int
  , balance :: Int  -- in cents
  , accountType :: AccountType
  , withdrawalCount :: Int
  } deriving (Show)

-- Create a new account
newAccount :: Int -> AccountType -> Account
newAccount accNo accType = Account
  { accountNumber = accNo
  , balance = 0
  , accountType = accType
  , withdrawalCount = 0
  }

-- Format money as dollars and cents
formatMoney :: Int -> String
formatMoney cents =
  let dollars = cents `div` 100
      remainingCents = abs (cents `mod` 100)
  in printf "$%d.%02d" dollars remainingCents

-- Parse amount from string (e.g., "100.50" -> 10050 cents)
parseAmount :: String -> Maybe Int
parseAmount s =
  let trimmed = filter (/= ' ') s
      (isNegative, numStr) = case trimmed of
        ('-':rest) -> (True, rest)
        _ -> (False, trimmed)
      parts = splitOn '.' numStr
  in case parts of
    [dollars] -> do
      d <- readMaybe dollars :: Maybe Int
      return $ (if isNegative then negate else id) (d * 100)
    [dollars, cents] -> do
      d <- readMaybe dollars :: Maybe Int
      let centsPadded = take 2 (cents ++ "00")
      c <- readMaybe centsPadded :: Maybe Int
      let total = d * 100 + c
      return $ if isNegative then negate total else total
    _ -> Nothing
  where
    splitOn :: Char -> String -> [String]
    splitOn delim str = case break (== delim) str of
      (before, "") -> [before]
      (before, _:after) -> before : splitOn delim after

-- Deposit money (pure function, returns new account)
deposit :: Int -> Account -> Account
deposit amount acc = acc { balance = balance acc + amount }

-- Checking withdrawal with service fee
withdrawChecking :: Int -> Account -> IO Account
withdrawChecking amount acc@Account{..}
  | balance < amount = do
      putStrLn $ "Insufficient funds: balance " ++ formatMoney balance
                 ++ ", check " ++ formatMoney amount
      return acc
  | otherwise = do
      let newBalance = balance - amount
          finalBalance = if newBalance < 50000  -- $500.00
                         then newBalance - 20   -- $0.20 fee
                         else newBalance
      return acc { balance = finalBalance }

-- Savings withdrawal with fee after first withdrawal
withdrawSavings :: Int -> Account -> IO Account
withdrawSavings amount acc@Account{..}
  | balance < amount = do
      putStrLn $ "Insufficient funds: balance " ++ formatMoney balance
                 ++ ", withdrawal " ++ formatMoney amount
      return acc
  | otherwise = do
      let newWithdrawalCount = withdrawalCount + 1
          balanceAfterFee = if newWithdrawalCount > 1
                           then balance - 500  -- $5.00 fee
                           else balance
          finalBalance = balanceAfterFee - amount
      return acc { balance = finalBalance
                 , withdrawalCount = newWithdrawalCount }

-- Polymorphic withdrawal based on account type
withdrawal :: Int -> Account -> IO Account
withdrawal amount acc = case accountType acc of
  Checking -> withdrawChecking amount acc
  Savings -> withdrawSavings amount acc

-- Display account information
displayAccount :: Account -> IO ()
displayAccount Account{..} = do
  let typeStr = case accountType of
        Checking -> "Checking"
        Savings -> "Savings"
  putStrLn $ typeStr ++ " Account " ++ show accountNumber
             ++ " = " ++ formatMoney balance

-- Read account number from user
promptAndReadAccountNo :: IO Int
promptAndReadAccountNo = do
  putStr "Enter account number: "
  hFlush stdout  -- Force output before reading
  input <- getLine
  case readMaybe input of
    Just n -> return n
    Nothing -> do
      putStrLn "Invalid number, using 0"
      return 0

-- Process transactions for an account
processAccount :: Account -> IO Account
processAccount initialAcc = do
  putStrLn "Enter positive number for deposit (e.g., 100.50),"
  putStrLn "negative for withdrawal (e.g., -25.00), 0 to terminate"
  loop initialAcc
  where
    loop acc = do
      putStr ": "
      hFlush stdout  -- Force output before reading
      input <- getLine
      case parseAmount input of
        Nothing -> do
          putStrLn "Invalid input, try again"
          loop acc
        Just transaction
          | transaction == 0 -> return acc
          | transaction > 0 -> loop (deposit transaction acc)
          | otherwise -> do
              updatedAcc <- withdrawal (negate transaction) acc
              loop updatedAcc

-- Main banking state
data BankState = BankState
  { checkingAccounts :: [Account]
  , savingsAccounts :: [Account]
  } deriving (Show)

emptyBankState :: BankState
emptyBankState = BankState [] []

-- Main banking loop
bankingLoop :: BankState -> IO BankState
bankingLoop state = do
  putStrLn "Enter S for Savings, C for Checking, X for exit"
  input <- getLine
  let choice = map toLower (filter (/= ' ') input)
  
  case choice of
    ('c':_) -> do
      accNo <- promptAndReadAccountNo
      let acc = newAccount accNo Checking
      processedAcc <- processAccount acc
      bankingLoop state { checkingAccounts = processedAcc : checkingAccounts state }
    
    ('s':_) -> do
      accNo <- promptAndReadAccountNo
      let acc = newAccount accNo Savings
      processedAcc <- processAccount acc
      bankingLoop state { savingsAccounts = processedAcc : savingsAccounts state }
    
    ('x':_) -> return state
    
    _ -> do
      putStrLn "I didn't get that."
      bankingLoop state

-- Calculate total balance
calculateTotal :: [Account] -> Int
calculateTotal = sum . map balance

-- Display all account totals
displayTotals :: BankState -> IO ()
displayTotals BankState{..} = do
  putStrLn "\nAccount totals:"
  
  -- Display checking accounts
  mapM_ displayAccount checkingAccounts
  let checkingTotal = calculateTotal checkingAccounts
  putStrLn $ "Total of all checking accounts = " ++ formatMoney checkingTotal
  
  -- Display savings accounts
  mapM_ displayAccount savingsAccounts
  let savingsTotal = calculateTotal savingsAccounts
  putStrLn $ "Total of all savings accounts = " ++ formatMoney savingsTotal
  
  -- Display grand total
  let grandTotal = checkingTotal + savingsTotal
  putStrLn $ "Total worth of all accounts   = " ++ formatMoney grandTotal

-- Main program
main :: IO ()
main = do
  finalState <- bankingLoop emptyBankState
  displayTotals finalState