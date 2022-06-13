package queue;

public class MyArrayQueueModuleTest {
    public static void main(String[] args) {
        ArrayQueueModule queue = new ArrayQueueModule();
        for (int i = 0; i < 10; i++) {
            queue.enqueue(i);
            System.out.println("size: " + queue.size());
        }
        for (int i = 0; i < 10; i++) {
            System.out.println(queue.dequeue() + " size: " + queue.size());
        }
    }
}
