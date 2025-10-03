// Modernized version largely written by ChatGPT of an example from 'C++ for Dummies' by Stephen R. Davis, published in 1994 (IDG Books)

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

// ---------------------- Account Types ------------------
struct Checking {
    Checking(unsigned accNo, float initialBalance = 0.0f)
        : accountNumber(accNo), balance(initialBalance) {}

    unsigned accountNo() const noexcept { return accountNumber; }
    float acntBalance() const noexcept { return balance; }

    void deposit(float amount) { balance += amount; }

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

private:
    unsigned accountNumber{};
    float balance{};
};

struct Savings {
    Savings(unsigned accNo, float initialBalance = 0.0f)
        : accountNumber(accNo), balance(initialBalance) {}

    unsigned accountNo() const noexcept { return accountNumber; }
    float acntBalance() const noexcept { return balance; }

    void deposit(float amount) { balance += amount; }

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
    unsigned accountNumber{};
    float balance{};
    int noWithdrawals{0};
};

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
[[nodiscard]] unsigned getAccntNo() {
    unsigned accntNo{};
    std::cout << "Enter account number: ";
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

} // namespace finance

// ---------------------- Main ----------------------
int main() {
    using namespace finance;

    char accountType{};
    bool keepLooping{true};

    //std::cout << std::string(u8"TEMPLATE VERSION");

    while (keepLooping) {
        std::cout << "Enter S for Savings, C for Checking, X for exit\n";
        std::cin >> accountType;

        switch (std::tolower(static_cast<unsigned char>(accountType))) {
            case 'c': {
                auto& acc = AccountManager<Checking>::create(getAccntNo());
                processAccount(acc);
                break;
            }
            case 's': {
                auto& acc = AccountManager<Savings>::create(getAccntNo());
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
    float total{0.0f};

    float checkingTotal{0.0f};
    for (const auto& acc : AccountManager<Checking>::accounts()) {
        acc->display();
        checkingTotal += acc->acntBalance();
    }
    std::cout << "Total of all checking accounts = " << checkingTotal << "\n";
    total += checkingTotal;

    float savingsTotal{0.0f};
    for (const auto& acc : AccountManager<Savings>::accounts()) {
        acc->display();
        savingsTotal += acc->acntBalance();
    }
    std::cout << "Total of all savings accounts = " << savingsTotal << "\n";
    total += savingsTotal;

    std::cout << "Total worth of all accounts   = " << total << "\n";

    return 0;
}

