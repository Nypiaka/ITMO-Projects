import java.util.concurrent.ThreadLocalRandom
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicReferenceArray
/**
 * @author KHairullin Aleksandr
 */
class FlatCombiningQueue<E> : Queue<E> {
    private val queue = ArrayDeque<E>() // sequential queue
    private val combinerLock = AtomicBoolean(false) // unlocked initially
    private val tasksForCombiner = AtomicReferenceArray<Any?>(TASKS_FOR_COMBINER_SIZE)

    @Suppress("UNCHECKED_CAST")
    override fun enqueue(element: E) {
        while (true) {
            val ind = randomCellIndex()
            if (tasksForCombiner.compareAndSet(ind, null, element)) {
                while (true) {
                    if (combinerLock.compareAndSet(false, true)) {
                        for (i in 0 until TASKS_FOR_COMBINER_SIZE) {
                            val tsk = tasksForCombiner.get(i)
                            if (tsk == Dequeue) {
                                val rs = queue.removeFirstOrNull()
                                tasksForCombiner.set(i, Result(rs))
                            } else if (tsk !is Result<*> && tsk != null) {
                                val rs = queue.addLast(tsk as E)
                                tasksForCombiner.set(i, Result(rs))
                            }
                        }
                        combinerLock.set(false)
                    }
                    if (tasksForCombiner.get(ind) is Result<*>) {
                        tasksForCombiner.set(ind, null)
                        return
                    }
                }
            }
        }
    }

    @Suppress("UNCHECKED_CAST")
    override fun dequeue(): E? {
        while (true) {
            val ind = randomCellIndex()
            if (tasksForCombiner.compareAndSet(ind, null, Dequeue)) {
                while (true) {
                    if (combinerLock.compareAndSet(false, true)) {
                        for (i in 0 until TASKS_FOR_COMBINER_SIZE) {
                            val tsk = tasksForCombiner.get(i)
                            if (tsk == Dequeue) {
                                val rs = queue.removeFirstOrNull()
                                tasksForCombiner.set(i, Result(rs))
                            } else if (tsk !is Result<*> && tsk != null) {
                                val rs = queue.addLast(tsk as E)
                                tasksForCombiner.set(i, Result(rs))
                            }
                        }
                        combinerLock.set(false)
                    }
                    val r = tasksForCombiner.get(ind)
                    if (r is Result<*>) {
                        tasksForCombiner.set(ind, null)
                        return r.value as E?
                    }
                }
            }
        }
    }

    private fun randomCellIndex(): Int =
        ThreadLocalRandom.current().nextInt(tasksForCombiner.length())
}

private const val TASKS_FOR_COMBINER_SIZE = 3 // Do not change this constant!

// TODO: Put this token in `tasksForCombiner` for dequeue().
// TODO: enqueue()-s should put the inserting element.
private object Dequeue

// TODO: Put the result wrapped with `Result` when the operation in `tasksForCombiner` is processed.
private class Result<V>(
    val value: V
)