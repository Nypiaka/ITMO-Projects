import java.util.concurrent.ThreadLocalRandom
import java.util.concurrent.atomic.AtomicReferenceArray

/**
 * @author KHairullin Aleksandr
 */
open class TreiberStackWithElimination<E> : Stack<E> {
    private val stack = TreiberStack<E>()

    // TODO: Try to optimize concurrent push and pop operations,
    // TODO: synchronizing them in an `eliminationArray` cell.
    private val eliminationArray = AtomicReferenceArray<Any?>(ELIMINATION_ARRAY_SIZE)

    override fun push(element: E) {
        if (tryPushElimination(element)) return
        stack.push(element)
    }

    protected open fun tryPushElimination(element: E): Boolean {
        val ri = randomCellIndex()
        if (eliminationArray.compareAndSet(ri, CELL_STATE_EMPTY, element)) {
            for (i in 0 until ELIMINATION_WAIT_CYCLES) {
                if (eliminationArray.compareAndSet(ri, CELL_STATE_RETRIEVED, CELL_STATE_EMPTY)) {
                    return true
                }
            }
            if (eliminationArray.compareAndSet(ri, element, CELL_STATE_EMPTY)) {
                return false
            } else {
                eliminationArray.set(ri, CELL_STATE_EMPTY)
                return true
            }
        } else return false
    }

    override fun pop(): E? = tryPopElimination() ?: stack.pop()

    @Suppress("UNCHECKED_CAST")
    private fun tryPopElimination(): E? {
        val ri = randomCellIndex()
        val res = eliminationArray.getAndUpdate(
            ri
        ) { a ->
            if (a == CELL_STATE_EMPTY || a == CELL_STATE_RETRIEVED) {
                a
            } else {
                CELL_STATE_RETRIEVED
            }
        }
        if (res == CELL_STATE_RETRIEVED || res == CELL_STATE_EMPTY) return null
        return res as E
    }

    private fun randomCellIndex(): Int =
        ThreadLocalRandom.current().nextInt(eliminationArray.length())

    companion object {
        private const val ELIMINATION_ARRAY_SIZE = 2 // Do not change!
        private const val ELIMINATION_WAIT_CYCLES = 1 // Do not change!

        // Initially, all cells are in EMPTY state.
        private val CELL_STATE_EMPTY = null

        // `tryPopElimination()` moves the cell state
        // to `RETRIEVED` if the cell contains element.
        private val CELL_STATE_RETRIEVED = Any()
    }
}
