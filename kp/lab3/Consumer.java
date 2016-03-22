import java.util.concurrent.BlockingQueue;

public class Consumer extends Thread {
    private final BlockingQueue<Long> buffer;
    private final int delay;

    public Consumer(BlockingQueue<Long> buffer, int delay) {
        this.buffer = buffer;
        this.delay = delay;
    }

    @Override
    public void run() {
        while (true) {
            try {
                long number = buffer.take();
                Thread.sleep(delay);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        }
    }
}
