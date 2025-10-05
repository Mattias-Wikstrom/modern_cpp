-- This program can be run using 'lean --run accounts.lean'

-- Import the standard library module for advanced String operations (filter, startsWith, etc.)
import Init.Data.String

-- Bank Account System in Lean 4
-- Reorganized for correct compilation order.

/-- Account types: Checking or Savings. -/
inductive AccountType where
  | checking : AccountType
  | savings : AccountType
  deriving Repr, DecidableEq

instance : ToString AccountType where
  toString
    | .checking => "Checking"
    | .savings  => "Savings"

/-- Account data structure (immutable). Balance is in cents. -/
structure Account where
  accountNumber   : Int
  balance         : Int -- in cents
  accountType     : AccountType
  withdrawalCount : Nat
  deriving Repr, DecidableEq

/-- Format money as dollars and cents. FIX: Uses concatenation to avoid '\$' escape error. -/
def formatMoney (cents : Int) : String :=
  let dollars := cents / 100
  let remainingCents := (cents % 100).natAbs
  -- Use concatenation for '$' to avoid the 'invalid escape sequence' error
  "$" ++ s!"{dollars}.{(if remainingCents < 10 then "0" else "")}{remainingCents}"

instance : ToString Account where
  toString acc :=
    s!"{acc.accountType} Account {acc.accountNumber} (Balance: {formatMoney acc.balance})"

/-- Create a new account. -/
def newAccount (accNo : Int) (accType : AccountType) : Account :=
  { accountNumber := accNo
    balance := 0
    accountType := accType
    withdrawalCount := 0 }

/--
Splits a string by a delimiter character.
-/
partial def splitOn (delim : Char) (s : String) : List String :=
  match s.splitOn (toString delim) with
  | [""] => if s.contains delim then ["", ""] else [""]
  | list => list

/-- Parse amount from string (e.g., "100.50" -> 10050 cents). -/
def parseAmount (s : String) : Option Int := do
  -- This filter requires the import
  let trimmed := s.trim
  let (isNegative, numStr) :=
    if trimmed.startsWith "-" then (true, trimmed.drop 1) else (false, trimmed)
  let parts := splitOn '.' numStr

  match parts with
  | [dollars] => do
    let d : Int ← dollars.toInt?
    pure $ (if isNegative then -d else d) * 100
  | [dollars, cents] => do
    let d : Int ← dollars.toInt?
    -- Pad cents to 2 digits and take the first two
    let centsPadded := (cents ++ "00").take 2
    let c : Int ← centsPadded.toInt?
    let total := d * 100 + c
    pure $ if isNegative then -total else total
  | _ => none

/-- Deposit money (pure function, returns new account). -/
def deposit (amount : Int) (acc : Account) : Account :=
  { acc with balance := acc.balance + amount }

/-- Checking withdrawal with service fee (updates the account state via IO). -/
def withdrawChecking (amount : Int) (acc : Account) : IO Account := do
  if acc.balance < amount then
    IO.println s!"Insufficient funds: balance {formatMoney acc.balance}, check {formatMoney amount}"
    pure acc
  else
    let newBalance := acc.balance - amount
    let finalBalance :=
      if newBalance < 50000 then -- $500.00 threshold
        newBalance - 20          -- $0.20 fee
      else
        newBalance
    pure { acc with balance := finalBalance }

/-- Savings withdrawal with fee after first withdrawal (updates the account state via IO). -/
def withdrawSavings (amount : Int) (acc : Account) : IO Account := do
  if acc.balance < amount then
    IO.println s!"Insufficient funds: balance {formatMoney acc.balance}, withdrawal {formatMoney amount}"
    pure acc
  else
    let newWithdrawalCount := acc.withdrawalCount + 1
    let balanceAfterFee :=
      if newWithdrawalCount > 1 then
        acc.balance - 500 -- $5.00 fee
      else
        acc.balance
    let finalBalance := balanceAfterFee - amount
    pure { acc with balance := finalBalance, withdrawalCount := newWithdrawalCount }

/-- Polymorphic withdrawal based on account type. -/
def withdrawal (amount : Int) (acc : Account) : IO Account :=
  match acc.accountType with
  | .checking => withdrawChecking amount acc
  | .savings  => withdrawSavings amount acc

/-- Display account information. -/
def displayAccount (acc : Account) : IO Unit :=
  IO.println s!"{acc.accountType} Account {acc.accountNumber} = {formatMoney acc.balance}"

/-- Prompts the user for input and flushes stdout. -/
def prompt (message : String) : IO String := do
  IO.print message
  -- 'flush' is available in the basic IO environment
  -- IO.eprint (flush)
  -- 'IO.getLine' is available in the basic IO environment
  let stdin ← IO.getStdin
  stdin.getLine

/-- Read account number from user. -/
def promptAndReadAccountNo : IO Int := do
  let input ← prompt "Enter account number: "
  match input.toInt? with
  | some n => pure n
  | none =>
    IO.println "Invalid number, using 0"
    pure 0

/-- Process transactions for an account. -/
partial def processAccount (initialAcc : Account) : IO Account := do
  IO.println "Enter positive number for deposit (e.g., 100.50),"
  IO.println "negative for withdrawal (e.g., -25.00), 0 to terminate"

  let rec loop (acc : Account) : IO Account := do
    let input ← prompt ": "
    match parseAmount input with
    | none =>
      IO.println "Invalid input, try again"
      loop acc
    | some transaction =>
      if transaction == 0 then
        pure acc
      else if transaction > 0 then
        loop (deposit transaction acc)
      else -- transaction < 0 (withdrawal)
        let updatedAcc ← withdrawal (-transaction) acc
        loop updatedAcc

  loop initialAcc -- FIX: Ensures the loop starts

/-- Main banking state. -/
structure BankState where
  checkingAccounts : List Account
  savingsAccounts  : List Account
  deriving Repr

def emptyBankState : BankState :=
  { checkingAccounts := [], savingsAccounts := [] }

/-- Calculate total balance from a list of accounts. -/
def calculateTotal (accounts : List Account) : Int :=
  accounts.map (·.balance) |>.sum

/-- Display all account totals. DEFINED BEFORE main -/
def displayTotals (state : BankState) : IO Unit := do
  IO.println "\nAccount totals:"

  -- Display checking accounts
  state.checkingAccounts.forM displayAccount
  let checkingTotal := calculateTotal state.checkingAccounts
  IO.println s!"Total of all checking accounts = {formatMoney checkingTotal}"

  -- Display savings accounts
  state.savingsAccounts.forM displayAccount
  let savingsTotal := calculateTotal state.savingsAccounts
  IO.println s!"Total of all savings accounts  = {formatMoney savingsTotal}"

  -- Display grand total
  let grandTotal := checkingTotal + savingsTotal
  IO.println s!"Total worth of all accounts    = {formatMoney grandTotal}"

/-- Main banking loop. DEFINED BEFORE main -/
partial def bankingLoop (state : BankState) : IO BankState := do
  IO.println "Enter S for Savings, C for Checking, X for exit"
  let stdin ← IO.getStdin
  let input ← stdin.getLine
  -- This trim/toLower requires the import
  let choice := input.trim.toLower

  match choice.get? 0 with
  | some 'c' => do
    let accNo ← promptAndReadAccountNo
    let acc := newAccount accNo .checking
    let processedAcc ← processAccount acc
    bankingLoop { state with checkingAccounts := processedAcc :: state.checkingAccounts }

  | some 's' => do
    let accNo ← promptAndReadAccountNo
    let acc := newAccount accNo .savings
    let processedAcc ← processAccount acc
    bankingLoop { state with savingsAccounts := processedAcc :: state.savingsAccounts }

  | some 'x' => pure state

  | _ => do
    IO.println "I didn't get that."
    bankingLoop state

/-- Main program entry point. -/
def main : IO Unit := do
  let finalState ← bankingLoop emptyBankState
  displayTotals finalState

