-- Run with: lean --run accounts_refactored.lean
import Init.Data.String
open IO

/-- Type class for polymorphic account operations. -/
class AccountLike (α : Type) where
  deposit    : α → Int → α
  balance    : α → Int
  withdrawal : α → Int → IO α
  display    : α → IO Unit

-- -------------------- Account Types --------------------

/-- Checking account. -/
structure Checking where
  accountNumber : Int
  balance       : Int
  deriving Repr, DecidableEq

instance : AccountLike Checking where
  deposit c amt := { c with balance := c.balance + amt }
  balance c := c.balance
  withdrawal c amt := do
    if c.balance < amt then
      IO.println s!"Insufficient funds: balance {c.balance}, withdrawal {amt}"
      pure c
    else
      let newBal := c.balance - amt
      let finalBal := if newBal < 50000 then newBal - 20 else newBal
      pure { c with balance := finalBal }
  display c := IO.println s!"Checking Account {c.accountNumber} = {c.balance / 100}.{(c.balance % 100)}"

-- -------------------- Savings Account --------------------

/-- Savings account with withdrawal count. -/
structure Savings where
  accountNumber   : Int
  balance         : Int
  withdrawalCount : Nat
  deriving Repr, DecidableEq

instance : AccountLike Savings where
  deposit s amt := { s with balance := s.balance + amt }
  balance s := s.balance
  withdrawal s amt := do
    if s.balance < amt then
      IO.println s!"Insufficient funds: balance {s.balance}, withdrawal {amt}"
      pure s
    else
      let newCount := s.withdrawalCount + 1
      let fee := if newCount > 1 then 500 else 0
      pure { s with balance := s.balance - amt - fee, withdrawalCount := newCount }
  display s := IO.println s!"Savings Account {s.accountNumber} = {s.balance / 100}.{(s.balance % 100)}"

-- -------------------- Utilities --------------------

/-- Prompt user and read line. -/
def prompt (msg : String) : IO String := do
  IO.print msg
  let stdin ← IO.getStdin
  stdin.getLine

/-- Read account number from user. -/
def promptAndReadAccountNo : IO Int := do
  let input ← prompt "Enter account number: "
  match input.toInt? with
  | some n => pure n
  | none   => pure 0

/-- Parse amount (e.g., "100.50" -> 10050 cents). -/
def parseAmount (s : String) : Option Int := do
  let trimmed := s.trim
  let (neg, rest) := if trimmed.startsWith "-" then (true, trimmed.drop 1) else (false, trimmed)
  let parts := rest.splitOn "."
  match parts with
  | [d] => do let dollars ← d.toInt?; pure $ if neg then -dollars * 100 else dollars * 100
  | [d, c] => do
      let dollars ← d.toInt?
      let cents := (c ++ "00").take 2
      let centVal ← cents.toInt?
      let total := dollars * 100 + centVal
      pure $ if neg then -total else total
  | _ => none

/-- Process transactions for any AccountLike type. -/
partial def processAccount {α : Type} [AccountLike α] (acc : α) : IO α := do
  IO.println "Enter positive for deposit, negative for withdrawal, 0 to terminate"
  let rec loop (a : α) : IO α := do
    let input ← prompt ": "
    match parseAmount input with
    | none      => loop a
    | some 0    => pure a
    | some amt  =>
      if amt > 0 then loop (AccountLike.deposit a amt)
      else loop (← AccountLike.withdrawal a (-amt))
  loop acc

/-- Calculate total balance for a list of AccountLike accounts. -/
def calculateTotal {α : Type} [AccountLike α] (accounts : List α) : IO Int := do
  let mut total := 0
  for acc in accounts do
    AccountLike.display acc
    total := total + AccountLike.balance acc
  pure total

-- -------------------- Bank Containers --------------------

structure BankState where
  checkingAccounts : List Checking
  savingsAccounts  : List Savings
  deriving Repr

def emptyBankState : BankState :=
  { checkingAccounts := [], savingsAccounts := [] }

-- -------------------- Banking Loop --------------------
partial def bankingLoop (state : BankState) : IO BankState := do
  IO.println "Enter S for Savings, C for Checking, X to exit"
  let input ← (← IO.getStdin).getLine
  let choice := input.trim.toLower
  match choice.get? 0 with
  | some 'c' => do
      let accNo ← promptAndReadAccountNo
      let acc := { accountNumber := accNo, balance := 0 }
      let processed ← processAccount acc
      bankingLoop { state with checkingAccounts := processed :: state.checkingAccounts }
  | some 's' => do
      let accNo ← promptAndReadAccountNo
      let acc := { accountNumber := accNo, balance := 0, withdrawalCount := 0 }
      let processed ← processAccount acc
      bankingLoop { state with savingsAccounts := processed :: state.savingsAccounts }
  | some 'x' => pure state
  | _        => bankingLoop state

-- -------------------- Display Totals --------------------

def displayTotals (state : BankState) : IO Unit := do
  IO.println "\nAccount totals:"
  let cTotal ← calculateTotal state.checkingAccounts
  IO.println s!"Total of all checking accounts = {cTotal / 100}.{(cTotal % 100)}"
  let sTotal ← calculateTotal state.savingsAccounts
  IO.println s!"Total of all savings accounts = {sTotal / 100}.{(sTotal % 100)}"
  IO.println s!"Grand total = {(cTotal + sTotal) / 100}.{((cTotal + sTotal) % 100)}"

-- -------------------- Main --------------------

def main : IO Unit := do
  let finalState ← bankingLoop emptyBankState
  displayTotals finalState
