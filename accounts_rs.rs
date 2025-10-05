// Bank Account System in Rust; conversion done by Claude.ai
use std::io::{self, Write};

// Account trait defines the interface for all account types
trait Account {
    fn account_no(&self) -> u32;
    fn balance(&self) -> i64; // in cents
    fn deposit(&mut self, amount: i64);
    fn withdrawal(&mut self, amount: i64);
    fn display(&self);
}

// BaseAccount contains common account data
#[derive(Debug)]
struct BaseAccount {
    account_number: u32,
    balance: i64, // in cents
}

impl BaseAccount {
    fn new(account_number: u32, initial_balance: i64) -> Self {
        Self {
            account_number,
            balance: initial_balance,
        }
    }
}

// CheckingAccount with service fee below $500
#[derive(Debug)]
struct CheckingAccount {
    base: BaseAccount,
}

impl CheckingAccount {
    fn new(account_number: u32, initial_balance: i64) -> Self {
        Self {
            base: BaseAccount::new(account_number, initial_balance),
        }
    }
}

impl Account for CheckingAccount {
    fn account_no(&self) -> u32 {
        self.base.account_number
    }

    fn balance(&self) -> i64 {
        self.base.balance
    }

    fn deposit(&mut self, amount: i64) {
        self.base.balance += amount;
    }

    fn withdrawal(&mut self, amount: i64) {
        if self.base.balance < amount {
            println!(
                "Insufficient funds: balance {}, check {}",
                format_money(self.base.balance),
                format_money(amount)
            );
        } else {
            self.base.balance -= amount;
            if self.base.balance < 50000 {
                // $500.00 threshold
                self.base.balance -= 20; // $0.20 service fee
            }
        }
    }

    fn display(&self) {
        println!(
            "Checking Account {} = {}",
            self.base.account_number,
            format_money(self.base.balance)
        );
    }
}

// SavingsAccount with fee after first withdrawal
#[derive(Debug)]
struct SavingsAccount {
    base: BaseAccount,
    withdrawal_count: u32,
}

impl SavingsAccount {
    fn new(account_number: u32, initial_balance: i64) -> Self {
        Self {
            base: BaseAccount::new(account_number, initial_balance),
            withdrawal_count: 0,
        }
    }
}

impl Account for SavingsAccount {
    fn account_no(&self) -> u32 {
        self.base.account_number
    }

    fn balance(&self) -> i64 {
        self.base.balance
    }

    fn deposit(&mut self, amount: i64) {
        self.base.balance += amount;
    }

    fn withdrawal(&mut self, amount: i64) {
        if self.base.balance < amount {
            println!(
                "Insufficient funds: balance {}, withdrawal {}",
                format_money(self.base.balance),
                format_money(amount)
            );
        } else {
            self.withdrawal_count += 1;
            if self.withdrawal_count > 1 {
                self.base.balance -= 500; // $5.00 fee after first withdrawal
            }
            self.base.balance -= amount;
        }
    }

    fn display(&self) {
        println!(
            "Savings Account {} = {}",
            self.base.account_number,
            format_money(self.base.balance)
        );
    }
}

// AccountManager manages collections of accounts
struct AccountManager {
    checking_accounts: Vec<CheckingAccount>,
    savings_accounts: Vec<SavingsAccount>,
}

impl AccountManager {
    fn new() -> Self {
        Self {
            checking_accounts: Vec::new(),
            savings_accounts: Vec::new(),
        }
    }

    fn create_checking(&mut self, account_number: u32) -> &mut CheckingAccount {
        let account = CheckingAccount::new(account_number, 0);
        self.checking_accounts.push(account);
        self.checking_accounts.last_mut().unwrap()
    }

    fn create_savings(&mut self, account_number: u32) -> &mut SavingsAccount {
        let account = SavingsAccount::new(account_number, 0);
        self.savings_accounts.push(account);
        self.savings_accounts.last_mut().unwrap()
    }
}

// Utility functions

fn format_money(cents: i64) -> String {
    let dollars = cents / 100;
    let remaining_cents = (cents % 100).abs();
    format!("${}.{:02}", dollars, remaining_cents)
}

fn parse_amount(s: &str) -> Result<i64, String> {
    let s = s.trim();

    // Handle negative sign
    let (is_negative, num_str) = if let Some(stripped) = s.strip_prefix('-') {
        (true, stripped)
    } else {
        (false, s)
    };

    // Split on decimal point
    let parts: Vec<&str> = num_str.split('.').collect();

    let cents = match parts.len() {
        1 => {
            // No decimal point
            let dollars = parts[0]
                .parse::<i64>()
                .map_err(|_| "Invalid number format")?;
            dollars * 100
        }
        2 => {
            // Has decimal point
            let dollars = parts[0]
                .parse::<i64>()
                .map_err(|_| "Invalid number format")?;

            // Pad or truncate cents to 2 digits
            let cents_str = if parts[1].len() == 1 {
                format!("{}0", parts[1])
            } else {
                parts[1][..2.min(parts[1].len())].to_string()
            };

            let cents_part = cents_str
                .parse::<i64>()
                .map_err(|_| "Invalid cents format")?;

            dollars * 100 + cents_part
        }
        _ => return Err("Invalid amount format".to_string()),
    };

    Ok(if is_negative { -cents } else { cents })
}

fn prompt_and_read_account_no() -> io::Result<u32> {
    print!("Enter account number: ");
    io::stdout().flush()?;

    let mut input = String::new();
    io::stdin().read_line(&mut input)?;

    match input.trim().parse::<u32>() {
        Ok(n) => Ok(n),
        Err(_) => {
            println!("Invalid number, using 0");
            Ok(0)
        }
    }
}

fn read_line() -> io::Result<String> {
    let mut input = String::new();
    io::stdin().read_line(&mut input)?;
    Ok(input.trim().to_string())
}

fn process_account<T: Account>(account: &mut T) -> io::Result<()> {
    println!("Enter positive number for deposit (e.g., 100.50),");
    println!("negative for withdrawal (e.g., -25.00), 0 to terminate");

    loop {
        print!(": ");
        io::stdout().flush()?;

        let input = read_line()?;

        match parse_amount(&input) {
            Ok(transaction) => {
                if transaction == 0 {
                    break;
                } else if transaction > 0 {
                    account.deposit(transaction);
                } else {
                    account.withdrawal(-transaction);
                }
            }
            Err(e) => {
                println!("Invalid input: {}", e);
            }
        }
    }

    Ok(())
}

fn calculate_total<T: Account>(accounts: &[T]) -> i64 {
    accounts.iter().map(|acc| acc.balance()).sum()
}

fn display_totals(manager: &AccountManager) {
    println!("\nAccount totals:");

    // Display checking accounts
    for acc in &manager.checking_accounts {
        acc.display();
    }
    let checking_total = calculate_total(&manager.checking_accounts);
    println!(
        "Total of all checking accounts = {}",
        format_money(checking_total)
    );

    // Display savings accounts
    for acc in &manager.savings_accounts {
        acc.display();
    }
    let savings_total = calculate_total(&manager.savings_accounts);
    println!(
        "Total of all savings accounts = {}",
        format_money(savings_total)
    );

    // Display grand total
    let total = checking_total + savings_total;
    println!("Total worth of all accounts   = {}", format_money(total));
}

fn main() -> io::Result<()> {
    let mut manager = AccountManager::new();

    loop {
        println!("Enter S for Savings, C for Checking, X for exit");
        let choice = read_line()?.to_lowercase();

        if choice.is_empty() {
            continue;
        }

        match choice.chars().next().unwrap() {
            'c' => {
                let account_number = prompt_and_read_account_no()?;
                let account = manager.create_checking(account_number);
                process_account(account)?;
            }
            's' => {
                let account_number = prompt_and_read_account_no()?;
                let account = manager.create_savings(account_number);
                process_account(account)?;
            }
            'x' => {
                display_totals(&manager);
                break;
            }
            _ => {
                println!("I didn't get that.");
            }
        }
    }

    Ok(())
}
