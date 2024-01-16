import java.util.stream.IntStream
import kotlin.math.floor
import kotlin.math.sqrt

fun isPrime(n: Int): Boolean {
    return n > 1 && IntStream.range(2, sqrt(n.toDouble()).toInt() + 1).noneMatch { i -> n % i == 0 }
}

fun isPrime(n: Double): Boolean {
    if (n < 1.0) return false
    var i = 2.0
    while (i <= sqrt(n)) {
        if (n % floor(i) == 0.0) return false
        i++
    }
    return true
}


fun piFunction(x: Double): Int {
    var i = 2.0
    var counter = 0
    while (i < floor(x) + 1) {
        if (isPrime(i)) counter++
        i++
    }
    return counter
}
