#include <iostream>
#include <memory>
#include <vector>
#include <string>
#include <cctype>
#include <stdexcept>

namespace finance {

class Account {
public:
    Account(const Account&) = delete;
    Account& operator=(const Account&) = delete;
    virtual ~Account() = default;

    Account(unsigned accNo, float initialBalance = 0.0f)
        : accountNumber{accNo}, balance{initialBalance}
    {
        ++count;
    }

    [[nodiscard]] unsigned accountNo() const noexcept { return accountNumber; }
    [[nodiscard]] float acntBalance() const noexcept { return balance; }

    static int noAccounts() noexcept { return count; }

    void deposit(float amount) { balance += amount; }
    virtual void withdrawal(float amount) = 0;
    virtual void display() const {
        std::cout << "Account " << accountNumber << " = " << balance << "\n";
    }

protected:
    inline static int count{0};
    unsigned accountNumber{};
    float balance{};
};

template <typename T>
class AccountWithList : public Account {
public:
    using Container = std::vector<std::unique_ptr<T>>;

    static T& create(unsigned accNo, float initialBalance = 0.0f) {
        auto acc = std::make_unique<T>(accNo, initialBalance);
        auto& ref = *acc;
        accounts().push_back(std::move(acc));
        return ref;
    }

    static Container& accounts() {
        static Container list;
        return list;
    }

protected:
public:
    AccountWithList(unsigned accNo, float initialBalance)
        : Account(accNo, initialBalance) {}
};

// ---------------- Checking ----------------
class Checking final : public AccountWithList<Checking> {
public:
    using AccountWithList<Checking>::AccountWithList;

    void withdrawal(float amount) override {
        if (balance < amount) {
            std::cout << "Insufficient funds: balance " << balance
                      << ", check " << amount << "\n";
        } else {
            balance -= amount;
            if (balance < 500.0f) {
                balance -= 0.20f; // service fee
            }
        }
    }
};

// ---------------- Savings ----------------
class Savings final : public AccountWithList<Savings> {
public:
    using AccountWithList<Savings>::AccountWithList;

    void withdrawal(float amount) override {
        if (balance < amount) {
            std::cout << "Insufficient funds: balance " << balance
                      << ", withdrawal " << amount << "\n";
        } else {
            if (++noWithdrawals > 1) {
                balance -= 5.0f; // fee after first withdrawal
            }
            balance -= amount;
        }
    }

private:
    int noWithdrawals{0};
};

// ---------------- Utility Functions ----------------
[[nodiscard]] unsigned getAccntNo() {
    unsigned accntNo{};
    std::cout << "Enter account number: ";
    std::cin >> accntNo;
    return accntNo;
}

void process(Account& account) {
    std::cout << "Enter positive number for deposit,\n"
                 "negative for withdrawal, 0 to terminate";

    float transaction{};
    do {
        std::cout << ": ";
        std::cin >> transaction;

        if (transaction > 0.0f) {
            account.deposit(transaction);
        } else if (transaction < 0.0f) {
            account.withdrawal(-transaction);
        }
    } while (transaction != 0.0f);
}

} // namespace finance

// ---------------- Main Program ----------------
int main() {
    using namespace finance;

    char accountType{};
    bool keepLooping{true};

    while (keepLooping) {
        std::cout << "Enter S for Savings, C for Checking, X for exit\n";
        std::cin >> accountType;

        switch (std::tolower(static_cast<unsigned char>(accountType))) {
            case 'c': {
                auto& acc = Checking::create(getAccntNo());
                process(acc);
                break;
            }
            case 's': {
                auto& acc = Savings::create(getAccntNo());
                process(acc);
                break;
            }
            case 'x':
                keepLooping = false;
                break;
            default:
                std::cout << "I didn't get that.\n";
        }
    }

    // Totals
    float total{0.0f};
    std::cout << "Account totals:\n";

    float checkingTotal{0.0f};
    for (const auto& acc : Checking::accounts()) {
        acc->display();
        checkingTotal += acc->acntBalance();
    }
    std::cout << "Total of all checking accounts = " << checkingTotal << "\n";
    total += checkingTotal;

    float savingsTotal{0.0f};
    for (const auto& acc : Savings::accounts()) {
        acc->display();
        savingsTotal += acc->acntBalance();
    }
    std::cout << "Total of all savings accounts = " << savingsTotal << "\n";
    total += savingsTotal;

    std::cout << "Total worth of all accounts   = " << total << "\n";
    return 0;
}

