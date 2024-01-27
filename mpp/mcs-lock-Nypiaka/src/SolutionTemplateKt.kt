import java.util.concurrent.atomic.AtomicReference

/**
 * @author KHairullin Aleksandr
 */
class Solution(val env: Environment) : Lock<Solution.Node> {
    private val tail = AtomicReference<Node?>(null)

    override fun lock(): Node {
        val my = Node() // сделали узел
        my.next.set(null)
        my.locked.set(true)
        val pred = tail.getAndSet(my)
        if (pred != null) {
            pred.next.set(my)
            while (my.locked.get()) {
                env.park()
            }
        }
        return my // вернули узел
    }

    override fun unlock(node: Node) {
        if (node.next.get() == null) {
            if (tail.compareAndSet(node, null)) {
                return
            } else {
                while (node.next.get() == null) {

                }
            }
        }
        node.next.get()?.locked?.set(false)
        env.unpark(node.next.get()!!.thread)
    }

    class Node {
        val thread = Thread.currentThread() // запоминаем поток, которые создал узел

        // todo: необходимые поля (val, используем AtomicReference)
        val locked = AtomicReference(false)
        val next = AtomicReference<Node?>(null)
    }
}