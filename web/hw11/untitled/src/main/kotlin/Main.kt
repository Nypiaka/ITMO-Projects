import java.math.BigInteger
import java.util.*


fun main() {

    val numbers: MutableList<String> = ArrayList<String>(listOf("first", "second", "third"))
    for (number in numbers) {
        if ("third" == number) {
            numbers.add("fourth")
        }
    }
    println(numbers.size)
}

fun min(bigInteger: BigInteger, bigInteger1: BigInteger): BigInteger {
    if (bigInteger <= bigInteger1) {
        return bigInteger
    }
    return bigInteger1
}
