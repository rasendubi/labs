import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ArrayBlockingQueue;

public class Main {
  private static final int DELAY1 = 300;
  private static final int DELAY2 = 500;
  private static final int DELAY3 = 800;

  public static void main(String[] args) throws InterruptedException {
    BlockingQueue<Long> buffer = new QueueWrapper<>(4);
    List<Runnable> consumers = Arrays.asList(
      new Consumer(buffer, DELAY1),
      new Consumer(buffer, DELAY2),
      new Consumer(buffer, DELAY3));

    List<Runnable> producers = Arrays.asList(
      new Producer(buffer),
      new Producer(buffer),
      new Producer(buffer));

    ExecutorService consumerExecutor = Executors.newFixedThreadPool(3);
    consumers.forEach(consumerExecutor::execute);

    ExecutorService producerExecutor = Executors.newFixedThreadPool(3);
    producers.forEach(producerExecutor::execute);
  }
}

class QueueWrapper<T> extends ArrayBlockingQueue<T> {
    public QueueWrapper(int size) {
        super(size);
    }

    public void put(T value) throws InterruptedException {
        synchronized (this) {
        System.out.println(Thread.currentThread().getName() + " put " + value);
        System.out.print("Before: ");
        System.out.println(this);
        System.out.println();
        }

        super.put(value);

        synchronized (this) {
        System.out.println(Thread.currentThread().getName() + " put " + value + " done");
        System.out.print("After:  ");
        System.out.println(this);
        System.out.println();
        }
    }

    public T take() throws InterruptedException {
        synchronized (this) {
        System.out.println(Thread.currentThread().getName() + " get");
        System.out.print("Before: ");
        System.out.println(this);
        System.out.println();
        }

        T value = super.take();

        synchronized (this) {
        System.out.println(Thread.currentThread().getName() + " got " + value);
        System.out.print("After:  ");
        System.out.println(this);
        System.out.println();
        }

        return value;
    }
}
