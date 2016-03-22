import java.util.concurrent.BlockingQueue;

public class Producer extends Thread {
    private final BlockingQueue<Long> buffer;

    public Producer(BlockingQueue<Long> buffer) {
        this.buffer = buffer;
    }

    @Override
    public void run() {
        for (int i = 0; i < 1000; ++i) {
            long number = stupidFib(i);
            try {
                buffer.put(number);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        }
    }

    private long stupidFib(int n) {
        if (n < 2) {
            return 1;
        } else {
            return stupidFib(n - 1) + stupidFib(n - 2);
        }
    }
}
