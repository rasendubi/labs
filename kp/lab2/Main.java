import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class Main {
  private static final int DELAY1 = 300;
  private static final int DELAY2 = 500;
  private static final int DELAY3 = 800;

  public static void main(String[] args) throws InterruptedException {
    SynchronizedBuffer<Long> buffer = new SynchronizedBuffer<>(4);
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
