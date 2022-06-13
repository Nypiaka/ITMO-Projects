package queue;

public class MyArrayQueueADTTest {
    public static void main(String[] args) {
        ArrayQueueADT queue = new ArrayQueueADT();
        for (int i = 0; i < 10; i++) {
            queue.enqueue(queue, i);
            System.out.println("size: " + queue.size(queue));
        }
        for (int i = 0; i < 10; i++) {
            System.out.println(queue.dequeue(queue) + " size: " + queue.size(queue));
        }
    }
}
