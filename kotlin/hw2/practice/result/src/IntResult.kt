import java.lang.RuntimeException

sealed interface IntResult {
    data class Ok(val value: Int) : IntResult
    data class Error(val reason: String) : IntResult

    fun getOrDefault(default: Int): Int = when (this) {
        is Error -> default
        is Ok -> value
    }

    fun getOrNull(): Int? = when (this) {
        is Error -> null
        is Ok -> value
    }

    fun getStrict(): Int = when (this) {
        is Error -> throw NoResultProvided(reason)
        is Ok -> value
    }
}


class NoResultProvided(message: String) : NoSuchElementException(message)

fun safeRun(function: () -> Int): IntResult {
    return try {
        IntResult.Ok(function())
    } catch (e: RuntimeException) {
        IntResult.Error(e.message!!)
    }
}
