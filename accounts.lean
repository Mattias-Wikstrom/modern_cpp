-- Run with: lean --run accounts.lean

import Init.Data.String
open IO

/-- Format money as dollars and cents. -/
def formatMoney (cents : Int) : String :=
  let dollars := cents / 100
  let remainingCents := (cents % 100).natAbs
  "$" ++ s!"{dollars}.{if remainingCents < 10 then "0" else ""}{remainingCents}"

/-- Account types as separate constructors. -/
inductive Account where
  | checking (accountNumber : Int) (balance : Int) (withdrawalCount : Nat)
  | savings  (accountNumber : Int) (balance : Int)
  deriving Repr, DecidableEq

instance : ToString Account where
  toString
    | .checking n bal _ => s!"Checking Account {n} (Balance: {formatMoney bal})"
    | .savings n bal    => s!"Savings Account {n} (Balance: {formatMoney bal})"

/-- Create new accounts. -/
def newChecking (n : Int) : Account := .checking n 0 0
def newSavings  (n : Int) : Account := .savings n 0

/-- Split a string on a delimiter. -/
def splitOn (delim : Char) (s : String) : List String :=
  s.splitOn (toString delim)

/-- Parse amount (e.g. "100.50" → 10050 cents). -/
def parseAmount (s : String) : Option Int := do
  let trimmed := s.trim
  let (isNeg, numStr) :=
    if trimmed.startsWith "-" then (true, trimmed.drop 1) else (false, trimmed)
  let parts := splitOn '.' numStr
  match parts with
  | [dollars] =>
      let d ← dollars.toInt?
      pure ((if isNeg then -d else d) * 100)
  | [dollars, cents] =>
      let d ← dollars.toInt?
      let centsPadded := (cents ++ "00").take 2
      let c ← centsPadded.toInt?
      let total := d * 100 + c
      pure (if isNeg then -total else total)
  | _ => none

/-- Deposit money (pure). -/
def deposit (amount : Int) : Account → Account
  | .checking n bal w => .checking n (bal + amount) w
  | .savings n bal    => .savings n (bal + amount)

/-- Withdraw from checking (service fee if balance < $500). -/
def withdrawChecking (amount : Int) : Account → IO Account
  | .checking n bal w =>
    if bal < amount then
      do IO.println s!"Insufficient funds: balance {formatMoney bal}, withdrawal {formatMoney amount}"
         pure (.checking n bal w)
    else
      let newBal := bal - amount
      let finalBal := if newBal < 50000 then newBal - 20 else newBal
      pure (.checking n finalBal (w + 1))
  | acc => pure acc

/-- Withdraw from savings (fee after first withdrawal). -/
def withdrawSavings (amount : Int) (count : Nat) : Account → IO (Account × Nat)
  | .savings n bal =>
    if bal < amount then
      do IO.println s!"Insufficient funds: balance {formatMoney bal}, withdrawal {formatMoney amount}"
         pure (.savings n bal, count)
    else
      let fee := if count > 0 then 500 else 0
      let finalBal := bal - amount - fee
      pure (.savings n finalBal, count + 1)
  | acc => pure (acc, count)

/-- Withdraw polymorphically. -/
def withdraw (amount : Int) (acc : Account) (savingsCount : Nat) : IO (Account × Nat) :=
  match acc with
  | .checking .. =>
      do let acc' ← withdrawChecking amount acc
         pure (acc', savingsCount)
  | .savings .. =>
      withdrawSavings amount savingsCount acc

/-- Display account. -/
def displayAccount (acc : Account) : IO Unit :=
  IO.println s!"{acc}"

/-- Prompt user and read input. -/
def prompt (msg : String) : IO String := do
  IO.print msg
  let stdin ← IO.getStdin
  stdin.getLine

/-- Read account number. -/
def promptAndReadAccountNo : IO Int := do
  let input ← prompt "Enter account number: "
  match input.trim.toInt? with
  | some n => pure n
  | none =>
      IO.println "Invalid number, using 0"
      pure 0

/-- Process transactions for an account. -/
partial def processAccount (initialAcc : Account) (savingsCount : Nat) : IO (Account × Nat) := do
  IO.println "Enter positive number for deposit (e.g., 100.50),"
  IO.println "negative for withdrawal (e.g., -25.00), 0 to terminate"

  let rec loop (acc : Account) (count : Nat) : IO (Account × Nat) := do
    let input ← prompt ": "
    match parseAmount input with
    | none =>
        IO.println "Invalid input, try again"
        loop acc count
    | some 0 => pure (acc, count)
    | some amt =>
        if amt > 0 then
          loop (deposit amt acc) count
        else
          do let (newAcc, newCount) ← withdraw (-amt) acc count
             loop newAcc newCount
  loop initialAcc savingsCount

/-- Bank state. -/
structure BankState where
  checkingAccounts : List Account
  savingsAccounts  : List Account
  deriving Repr

def emptyBankState : BankState :=
  { checkingAccounts := [], savingsAccounts := [] }

/-- Calculate total balance. -/
def accountBalance : Account → Int
  | .checking _ bal _ => bal
  | .savings  _ bal   => bal

def calculateTotal (accounts : List Account) : Int :=
  (accounts.map accountBalance).foldl (· + ·) 0

/-- Display totals. -/
def displayTotals (state : BankState) : IO Unit := do
  IO.println "\nAccount totals:"
  IO.println "Checking accounts:"
  for acc in state.checkingAccounts do displayAccount acc
  let checkingTotal := calculateTotal state.checkingAccounts
  IO.println s!"Total of all checking accounts = {formatMoney checkingTotal}"

  IO.println "\nSavings accounts:"
  for acc in state.savingsAccounts do displayAccount acc
  let savingsTotal := calculateTotal state.savingsAccounts
  IO.println s!"Total of all savings accounts = {formatMoney savingsTotal}"

  IO.println s!"\nTotal worth of all accounts  = {formatMoney (checkingTotal + savingsTotal)}"

/-- Main banking loop. -/
partial def bankingLoop (state : BankState) (savingsCount : Nat) : IO BankState := do
  IO.println "Enter S for Savings, C for Checking, X for exit"
  let input ← (← IO.getStdin).getLine
  let choice := input.trim.toLower
  match choice.get? 0 with
  | some 'c' => do
      let accNo ← promptAndReadAccountNo
      let acc := newChecking accNo
      let (processedAcc, _) ← processAccount acc 0
      bankingLoop { state with checkingAccounts := processedAcc :: state.checkingAccounts } savingsCount
  | some 's' => do
      let accNo ← promptAndReadAccountNo
      let acc := newSavings accNo
      let (processedAcc, newCount) ← processAccount acc savingsCount
      bankingLoop { state with savingsAccounts := processedAcc :: state.savingsAccounts } newCount
  | some 'x' => pure state
  | _ =>
      IO.println "I didn't get that."
      bankingLoop state savingsCount

/-- Main entry point. -/
def main : IO Unit := do
  let finalState ← bankingLoop emptyBankState 0
  displayTotals finalState
