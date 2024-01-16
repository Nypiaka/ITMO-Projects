class BankAccount(amount: Int) {
    var balance = 0
        private set(newBalance) {
            logTransaction(field, newBalance)
            field = newBalance
        }

    init {
        require(amount > 0) { "Amount should be more than 0" }
        balance = amount
    }

    fun withdraw(amount: Int) {
        require(amount in 1..<balance) { "Amount should be more than 0 and less than balance" }
        balance -= amount
    }

    fun deposit(amount: Int) {
        require(amount > 0) { "Amount should be more than 0" }
        balance += amount
    }
}
