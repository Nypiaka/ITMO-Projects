fun fibonacciFor(n: Int): Int {
    var f1 = 0
    var f2 = 1
    for (i in 0 until n) {
        f2 += f1
        f1 = f2 - f1
    }
    return f1
}

fun fibonacciIf(n: Int): Int {
    return if (n < 2) n else fibonacciIf(n - 1) + fibonacciIf(n - 2)
}

fun fibonacciWhen(n: Int): Int {
    return when (n) {
        0 -> 0
        1 -> 1
        else -> fibonacciWhen(n - 1) + fibonacciWhen(n - 2)
    }
}
