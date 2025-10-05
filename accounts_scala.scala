// Bank Account System in Scala; Conversion by Claude.ai
import scala.io.StdIn
import scala.util.Try

// Utility functions accessible everywhere
object Utils {
  // Format money as dollars and cents
  def formatMoney(cents: Int): String = {
    val dollars = cents / 100
    val remainingCents = Math.abs(cents % 100)
    s"$$$dollars.${f"$remainingCents%02d"}"
  }
}

// Account trait defines the interface for all account types
sealed trait Account {
  def accountNo: Int
  def balance: Int  // in cents
  def deposit(amount: Int): Account
  def withdrawal(amount: Int): Account
  def display(): Unit
}

// Base account data (immutable case class)
case class AccountData(
  accountNumber: Int,
  balanceValue: Int  // in cents
)

// Checking account with service fee below $500
case class CheckingAccount(
  data: AccountData
) extends Account {
  
  def accountNo: Int = data.accountNumber
  def balance: Int = data.balanceValue
  
  def deposit(amount: Int): CheckingAccount = 
    copy(data = data.copy(balanceValue = data.balanceValue + amount))
  
  def withdrawal(amount: Int): CheckingAccount = {
    if (data.balanceValue < amount) {
      println(s"Insufficient funds: balance ${Utils.formatMoney(data.balanceValue)}, check ${Utils.formatMoney(amount)}")
      this
    } else {
      val newBalance = data.balanceValue - amount
      val finalBalance = if (newBalance < 50000) newBalance - 20 else newBalance  // $500.00 threshold, $0.20 fee
      copy(data = data.copy(balanceValue = finalBalance))
    }
  }
  
  def display(): Unit = {
    println(s"Checking Account ${data.accountNumber} = ${Utils.formatMoney(data.balanceValue)}")
  }
}

// Savings account with fee after first withdrawal
case class SavingsAccount(
  data: AccountData,
  withdrawalCount: Int = 0
) extends Account {
  
  def accountNo: Int = data.accountNumber
  def balance: Int = data.balanceValue
  
  def deposit(amount: Int): SavingsAccount = 
    copy(data = data.copy(balanceValue = data.balanceValue + amount))
  
  def withdrawal(amount: Int): SavingsAccount = {
    if (data.balanceValue < amount) {
      println(s"Insufficient funds: balance ${Utils.formatMoney(data.balanceValue)}, withdrawal ${Utils.formatMoney(amount)}")
      this
    } else {
      val newWithdrawalCount = withdrawalCount + 1
      val balanceAfterFee = if (newWithdrawalCount > 1) data.balanceValue - 500 else data.balanceValue  // $5.00 fee
      val finalBalance = balanceAfterFee - amount
      copy(
        data = data.copy(balanceValue = finalBalance),
        withdrawalCount = newWithdrawalCount
      )
    }
  }
  
  def display(): Unit = {
    println(s"Savings Account ${data.accountNumber} = ${Utils.formatMoney(data.balanceValue)}")
  }
}

// Account manager to hold collections
case class BankState(
  checkingAccounts: List[CheckingAccount] = List.empty,
  savingsAccounts: List[SavingsAccount] = List.empty
)

object BankAccount {
  
  // Parse amount from string (e.g., "100.50" -> 10050 cents)
  def parseAmount(s: String): Option[Int] = {
    try {
      val trimmed = s.trim
      val (isNegative, numStr) = if (trimmed.startsWith("-")) {
        (true, trimmed.drop(1))
      } else {
        (false, trimmed)
      }
      
      val parts = numStr.split('.')
      val cents = parts.length match {
        case 1 =>
          // No decimal point
          val dollars = parts(0).toInt
          Some(dollars * 100)
        case 2 =>
          // Has decimal point
          val dollars = parts(0).toInt
          val centsPadded = if (parts(1).length == 1) parts(1) + "0" else parts(1).take(2)
          val centsValue = centsPadded.toInt
          Some(dollars * 100 + centsValue)
        case _ => None
      }
      
      cents.map(c => if (isNegative) -c else c)
    } catch {
      case _: NumberFormatException => None
    }
  }
  
  // Read account number from user
  def promptAndReadAccountNo(): Int = {
    print("Enter account number: ")
    Try(StdIn.readLine().trim.toInt).getOrElse {
      println("Invalid number, using 0")
      0
    }
  }
  
  // Process transactions for an account
  def processAccount[A <: Account](account: A): A = {
    println("Enter positive number for deposit (e.g., 100.50),")
    println("negative for withdrawal (e.g., -25.00), 0 to terminate")
    
    @scala.annotation.tailrec
    def loop(acc: A): A = {
      print(": ")
      val input = StdIn.readLine()
      
      parseAmount(input) match {
        case None =>
          println("Invalid input, try again")
          loop(acc)
        case Some(transaction) if transaction == 0 =>
          acc
        case Some(transaction) if transaction > 0 =>
          loop(acc.deposit(transaction).asInstanceOf[A])
        case Some(transaction) =>
          loop(acc.withdrawal(-transaction).asInstanceOf[A])
      }
    }
    
    loop(account)
  }
  
  // Main banking loop
  @scala.annotation.tailrec
  def bankingLoop(state: BankState): BankState = {
    println("Enter S for Savings, C for Checking, X for exit")
    val input = StdIn.readLine().trim.toLowerCase
    
    if (input.isEmpty) {
      bankingLoop(state)
    } else {
      input.head match {
        case 'c' =>
          val accNo = promptAndReadAccountNo()
          val account = CheckingAccount(AccountData(accNo, 0))
          val processedAccount = processAccount(account)
          bankingLoop(state.copy(checkingAccounts = processedAccount :: state.checkingAccounts))
        
        case 's' =>
          val accNo = promptAndReadAccountNo()
          val account = SavingsAccount(AccountData(accNo, 0))
          val processedAccount = processAccount(account)
          bankingLoop(state.copy(savingsAccounts = processedAccount :: state.savingsAccounts))
        
        case 'x' =>
          state
        
        case _ =>
          println("I didn't get that.")
          bankingLoop(state)
      }
    }
  }
  
  // Calculate total balance
  def calculateTotal(accounts: List[Account]): Int = {
    accounts.map(_.balance).sum
  }
  
  // Display all account totals
  def displayTotals(state: BankState): Unit = {
    println("\nAccount totals:")
    
    // Display checking accounts
    state.checkingAccounts.foreach(_.display())
    val checkingTotal = calculateTotal(state.checkingAccounts)
    println(s"Total of all checking accounts = ${Utils.formatMoney(checkingTotal)}")
    
    // Display savings accounts
    state.savingsAccounts.foreach(_.display())
    val savingsTotal = calculateTotal(state.savingsAccounts)
    println(s"Total of all savings accounts = ${Utils.formatMoney(savingsTotal)}")
    
    // Display grand total
    val total = checkingTotal + savingsTotal
    println(s"Total worth of all accounts   = ${Utils.formatMoney(total)}")
  }
  
  // Main program
  def main(args: Array[String]): Unit = {
    val finalState = bankingLoop(BankState())
    displayTotals(finalState)
  }
}