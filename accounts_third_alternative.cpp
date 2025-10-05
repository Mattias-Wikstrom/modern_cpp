
// Version that combines features of the other two versions; suggested by Claude.ai

#include <iostream>
#include <memory>
#include <vector>
#include <cctype>
#include <string>

namespace finance {

// ---------------------- Concepts ----------------------
template <typename T>
concept AccountLike = requires(T a, float amount) {
    { a.accountNo() } -> std::convertible_to<unsigned>;
    { a.acntBalance() } -> std::convertible_to<float>;
    { a.deposit(amount) };
    { a.withdrawal(amount) };
    { a.display() };
};

// ---------------------- Base Account Data ----------------------
// Simple struct to hold common data - no virtual functions
struct AccountData {
    unsigned accountNumber{};
    float balance{};

    AccountData(unsigned accNo, float initialBalance = 0.0f)
        : accountNumber(accNo), balance(initialBalance) {}

    unsigned accountNo() const noexcept { return accountNumber; }
    float acntBalance() const noexcept { return balance; }
    
    void deposit(float amount) { balance += amount; }
};

// ---------------------- Account Types ------------------
struct Checking : AccountData {
    using AccountData::AccountData; // Inherit constructor

    void withdrawal(float amount) {
        if (balance < amount) {
            std::cout << "Insufficient funds: balance " << balance
                      << ", check " << amount << "\n";
        } else {
            balance -= amount;
            if (balance < 500.0f) balance -= 0.20f; // service fee
        }
    }

    void display() const {
        std::cout << "Checking Account " << accountNumber
                  << " = " << balance << "\n";
    }
};

struct Savings : AccountData {
    using AccountData::AccountData; // Inherit constructor

    void withdrawal(float amount) {
        if (balance < amount) {
            std::cout << "Insufficient funds: balance " << balance
                      << ", withdrawal " << amount << "\n";
        } else {
            if (++noWithdrawals > 1) balance -= 5.0f;
            balance -= amount;
        }
    }

    void display() const {
        std::cout << "Savings Account " << accountNumber
                  << " = " << balance << "\n";
    }

private:
    int noWithdrawals{0};
};

// Verify our types satisfy the concept
static_assert(AccountLike<Checking>);
static_assert(AccountLike<Savings>);

// ---------------------- Factory Containers ------------------
template <AccountLike T>
struct AccountManager {
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
};

// ---------------------- Utilities ----------------------
[[nodiscard]] unsigned promptAndReadAccountNo() {
    std::cout << "Enter account number: ";
    unsigned accntNo{};
    std::cin >> accntNo;
    return accntNo;
}

template <AccountLike T>
void processAccount(T& account) {
    std::cout << "Enter positive number for deposit,\n"
                 "negative for withdrawal, 0 to terminate";

    float transaction{};
    do {
        std::cout << ": ";
        std::cin >> transaction;
        if (transaction > 0.0f) account.deposit(transaction);
        else if (transaction < 0.0f) account.withdrawal(-transaction);
    } while (transaction != 0);
}

// Generic total calculator for any AccountLike container
template <AccountLike T>
float calculateTotal(const typename AccountManager<T>::Container& accounts) {
    float total{0.0f};
    for (const auto& acc : accounts) {
        acc->display();
        total += acc->acntBalance();
    }
    return total;
}

} // namespace finance

// ---------------------- Main ----------------------
int main() {
    using namespace finance;

    char accountType{};
    bool keepLooping{true};

    while (keepLooping) {
        std::cout << "Enter S for Savings, C for Checking, X for exit\n";
        std::cin >> accountType;

        switch (std::tolower(static_cast<unsigned char>(accountType))) {
            case 'c': {
                auto& acc = AccountManager<Checking>::create(promptAndReadAccountNo());
                processAccount(acc);
                break;
            }
            case 's': {
                auto& acc = AccountManager<Savings>::create(promptAndReadAccountNo());
                processAccount(acc);
                break;
            }
            case 'x':
                keepLooping = false;
                break;
            default:
                std::cout << "I didn't get that.\n";
        }
    }

    // ---------------- Totals ----------------
    std::cout << "\nAccount totals:\n";
    
    float checkingTotal = calculateTotal<Checking>(
        AccountManager<Checking>::accounts()
    );
    std::cout << "Total of all checking accounts = " << checkingTotal << "\n";

    float savingsTotal = calculateTotal<Savings>(
        AccountManager<Savings>::accounts()
    );
    std::cout << "Total of all savings accounts = " << savingsTotal << "\n";

    float total = checkingTotal + savingsTotal;
    std::cout << "Total worth of all accounts   = " << total << "\n";

    return 0;
}
