sealed interface Result<out T, out V> where T : Number, V : CharSequence {
    data class Ok<out T : Number>(val value: T) : Result<T, CharSequence>
    data class Error<out V : CharSequence>(val error: V) : Result<Number, V>

}