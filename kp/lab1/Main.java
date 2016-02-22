public class Main {
  public static void main(String[] args) throws InterruptedException {
    SynchronizedBuffer<Long> buffer = new SynchronizedBuffer<>(1);
    Consumer consumer = new Consumer(buffer);
    Producer producer = new Producer(buffer);

    consumer.start();
    producer.start();

    consumer.join();
    producer.join();
  }
}
