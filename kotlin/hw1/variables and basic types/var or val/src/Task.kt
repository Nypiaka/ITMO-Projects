fun playerTurn(turnNumber: Int): Int {
    val playersCount = 4
    return (turnNumber % playersCount)
}

fun factorial(n: Int): Int {
    var ans = 1
    for (i in 2..n) {
        ans *= i
    }
    return ans
}
