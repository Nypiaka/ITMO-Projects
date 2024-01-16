interface Value<T> {
    class ObserverCanceler<T>(private var observers: MutableSet<(T) -> Unit>, private val observer: (T) -> Unit) {
        fun cancel() {
            observers.remove(observer)
        }
    }

    fun observe(observer: (T) -> Unit): ObserverCanceler<T>
}

class MutableValue<T>(initial: T) : Value<T> {
    var value: T = initial
        set(newValue) {
            field = newValue
            observers.forEach { observer ->
                executeSafety(observer, newValue)
            }
        }

    private val observers = HashSet<(T) -> Unit>()

    override fun observe(observer: (T) -> Unit): Value.ObserverCanceler<T> {
        observers.add(observer)
        executeSafety(observer, value)
        return Value.ObserverCanceler(observers, observer)
    }

    private fun executeSafety(observer: (T) -> Unit, value: T) {
        try {
            observer(value)
        } catch (e: Exception) {
            println(e.message)
        }
    }
}
