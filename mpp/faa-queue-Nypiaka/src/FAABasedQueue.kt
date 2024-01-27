import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.atomic.AtomicReferenceArray

/**
 * @author KHairullin Aleksandr
 *
 * TODO: Copy the code from `FAABasedQueueSimplified`
 * TODO: and implement the infinite array on a linked list
 * TODO: of fixed-size `Segment`s.
 */
class FAABasedQueue<E> : Queue<E> {
    private val enqIdx = AtomicLong(0)
    private val deqIdx = AtomicLong(0)
    private val head: AtomicReference<Segment>
    private val tail: AtomicReference<Segment>
    private val fst = Segment(0)

    init {
        head = AtomicReference(fst)
        tail = AtomicReference(fst)
    }

    private fun findSegment(start: Segment, i: Long): Segment {
        var cf = fst
        if (start.id == i / SEGMENT_SIZE) {
            cf = start
        } else if (start.id < i / SEGMENT_SIZE) {
            cf = start
            while (true) {
                if (cf.id == i / SEGMENT_SIZE) {
                    break
                } else {
                    cf.next.compareAndSet(null, Segment(cf.id + 1))
                    cf = cf.next.get()!!
                }
            }
        } else {
            while (true) {
                if (cf.id == i / SEGMENT_SIZE) {
                    break
                } else {
                    cf = cf.next.get()!!
                }
            }
        }
        return cf
    }

    private fun move(cf: Segment, start: Segment, toChange: AtomicReference<Segment>) {
        if (cf.id > start.id) {
            cf.next.compareAndSet(null, Segment(cf.id + 1))
            toChange.set(
                cf
            )
        }
    }

    override fun enqueue(element: E) {
        while (true) {
            val curTail = tail.get()
            val i = enqIdx.getAndIncrement()
            val cf = findSegment(curTail, i)
            move(cf, curTail, tail)
            if (cf.cells.compareAndSet(
                    (i % SEGMENT_SIZE).toInt(),
                    null,
                    element
                )
            ) {
                return
            }
        }
    }

    @Suppress("UNCHECKED_CAST")
    override fun dequeue(): E? {
        while (true) {
            if (deqIdx.get() >= enqIdx.get()) return null
            val curHead = head.get()
            val i = deqIdx.getAndIncrement()
            val cf = findSegment(curHead, i)
            move(cf, curHead, head)
            if (!cf.cells.compareAndSet((i % SEGMENT_SIZE).toInt(), null, POISONED)) {
                return cf.cells.get((i % SEGMENT_SIZE).toInt()) as E
            }

        }
    }
}

private class Segment(val id: Long) {
    val next = AtomicReference<Segment?>(null)
    val cells = AtomicReferenceArray<Any?>(SEGMENT_SIZE)
}

// DO NOT CHANGE THIS CONSTANT
private const val SEGMENT_SIZE = 2

private val POISONED = Any()
