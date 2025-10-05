// Bank Account System in Go; conversion done by Claude.ai
package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

// AccountType represents the type of account
type AccountType int

const (
	Checking AccountType = iota
	Savings
)

// Account interface defines the contract for all account types
type Account interface {
	AccountNo() uint
	Balance() int // in cents
	Deposit(amount int)
	Withdrawal(amount int)
	Display()
}

// BaseAccount contains common account data
type BaseAccount struct {
	accountNumber uint
	balance       int // in cents
}

func (a *BaseAccount) AccountNo() uint {
	return a.accountNumber
}

func (a *BaseAccount) Balance() int {
	return a.balance
}

func (a *BaseAccount) Deposit(amount int) {
	a.balance += amount
}

// CheckingAccount with service fee below $500
type CheckingAccount struct {
	BaseAccount
}

func NewCheckingAccount(accNo uint, initialBalance int) *CheckingAccount {
	return &CheckingAccount{
		BaseAccount: BaseAccount{
			accountNumber: accNo,
			balance:       initialBalance,
		},
	}
}

func (c *CheckingAccount) Withdrawal(amount int) {
	if c.balance < amount {
		fmt.Printf("Insufficient funds: balance %s, check %s\n",
			formatMoney(c.balance), formatMoney(amount))
	} else {
		c.balance -= amount
		if c.balance < 50000 { // $500.00
			c.balance -= 20 // $0.20 service fee
		}
	}
}

func (c *CheckingAccount) Display() {
	fmt.Printf("Checking Account %d = %s\n", c.accountNumber, formatMoney(c.balance))
}

// SavingsAccount with fee after first withdrawal
type SavingsAccount struct {
	BaseAccount
	withdrawalCount int
}

func NewSavingsAccount(accNo uint, initialBalance int) *SavingsAccount {
	return &SavingsAccount{
		BaseAccount: BaseAccount{
			accountNumber: accNo,
			balance:       initialBalance,
		},
		withdrawalCount: 0,
	}
}

func (s *SavingsAccount) Withdrawal(amount int) {
	if s.balance < amount {
		fmt.Printf("Insufficient funds: balance %s, withdrawal %s\n",
			formatMoney(s.balance), formatMoney(amount))
	} else {
		s.withdrawalCount++
		if s.withdrawalCount > 1 {
			s.balance -= 500 // $5.00 fee after first withdrawal
		}
		s.balance -= amount
	}
}

func (s *SavingsAccount) Display() {
	fmt.Printf("Savings Account %d = %s\n", s.accountNumber, formatMoney(s.balance))
}

// AccountManager manages collections of accounts
type AccountManager struct {
	checkingAccounts []*CheckingAccount
	savingsAccounts  []*SavingsAccount
}

func NewAccountManager() *AccountManager {
	return &AccountManager{
		checkingAccounts: make([]*CheckingAccount, 0),
		savingsAccounts:  make([]*SavingsAccount, 0),
	}
}

func (am *AccountManager) CreateChecking(accNo uint) *CheckingAccount {
	acc := NewCheckingAccount(accNo, 0)
	am.checkingAccounts = append(am.checkingAccounts, acc)
	return acc
}

func (am *AccountManager) CreateSavings(accNo uint) *SavingsAccount {
	acc := NewSavingsAccount(accNo, 0)
	am.savingsAccounts = append(am.savingsAccounts, acc)
	return acc
}

// Utility functions

func formatMoney(cents int) string {
	dollars := cents / 100
	remainingCents := cents % 100
	if remainingCents < 0 {
		remainingCents = -remainingCents
	}
	return fmt.Sprintf("$%d.%02d", dollars, remainingCents)
}

func parseAmount(s string) (int, error) {
	s = strings.TrimSpace(s)
	
	// Handle negative sign
	isNegative := false
	if strings.HasPrefix(s, "-") {
		isNegative = true
		s = s[1:]
	}
	
	// Split on decimal point
	parts := strings.Split(s, ".")
	
	var cents int
	switch len(parts) {
	case 1:
		// No decimal point
		dollars, err := strconv.Atoi(parts[0])
		if err != nil {
			return 0, err
		}
		cents = dollars * 100
	case 2:
		// Has decimal point
		dollars, err := strconv.Atoi(parts[0])
		if err != nil {
			return 0, err
		}
		
		// Pad or truncate cents to 2 digits
		centStr := parts[1]
		if len(centStr) == 1 {
			centStr += "0"
		} else if len(centStr) > 2 {
			centStr = centStr[:2]
		}
		
		centsPart, err := strconv.Atoi(centStr)
		if err != nil {
			return 0, err
		}
		
		cents = dollars*100 + centsPart
	default:
		return 0, fmt.Errorf("invalid amount format")
	}
	
	if isNegative {
		cents = -cents
	}
	
	return cents, nil
}

func promptAndReadAccountNo(scanner *bufio.Scanner) uint {
	fmt.Print("Enter account number: ")
	scanner.Scan()
	accNo, err := strconv.ParseUint(scanner.Text(), 10, 32)
	if err != nil {
		fmt.Println("Invalid number, using 0")
		return 0
	}
	return uint(accNo)
}

func processAccount(acc Account, scanner *bufio.Scanner) {
	fmt.Println("Enter positive number for deposit (e.g., 100.50),")
	fmt.Println("negative for withdrawal (e.g., -25.00), 0 to terminate")
	
	for {
		fmt.Print(": ")
		scanner.Scan()
		input := scanner.Text()
		
		transaction, err := parseAmount(input)
		if err != nil {
			fmt.Println("Invalid input, try again")
			continue
		}
		
		if transaction == 0 {
			break
		} else if transaction > 0 {
			acc.Deposit(transaction)
		} else {
			acc.Withdrawal(-transaction)
		}
	}
}

func calculateTotal(accounts []Account) int {
	total := 0
	for _, acc := range accounts {
		total += acc.Balance()
	}
	return total
}

func displayTotals(am *AccountManager) {
	fmt.Println("\nAccount totals:")
	
	// Display checking accounts
	checkingAccounts := make([]Account, len(am.checkingAccounts))
	for i, acc := range am.checkingAccounts {
		acc.Display()
		checkingAccounts[i] = acc
	}
	checkingTotal := calculateTotal(checkingAccounts)
	fmt.Printf("Total of all checking accounts = %s\n", formatMoney(checkingTotal))
	
	// Display savings accounts
	savingsAccounts := make([]Account, len(am.savingsAccounts))
	for i, acc := range am.savingsAccounts {
		acc.Display()
		savingsAccounts[i] = acc
	}
	savingsTotal := calculateTotal(savingsAccounts)
	fmt.Printf("Total of all savings accounts = %s\n", formatMoney(savingsTotal))
	
	// Display grand total
	total := checkingTotal + savingsTotal
	fmt.Printf("Total worth of all accounts   = %s\n", formatMoney(total))
}

func main() {
	manager := NewAccountManager()
	scanner := bufio.NewScanner(os.Stdin)
	
	for {
		fmt.Println("Enter S for Savings, C for Checking, X for exit")
		scanner.Scan()
		choice := strings.ToLower(strings.TrimSpace(scanner.Text()))
		
		if len(choice) == 0 {
			continue
		}
		
		switch choice[0] {
		case 'c':
			accNo := promptAndReadAccountNo(scanner)
			acc := manager.CreateChecking(accNo)
			processAccount(acc, scanner)
		case 's':
			accNo := promptAndReadAccountNo(scanner)
			acc := manager.CreateSavings(accNo)
			processAccount(acc, scanner)
		case 'x':
			displayTotals(manager)
			return
		default:
			fmt.Println("I didn't get that.")
		}
	}
}
