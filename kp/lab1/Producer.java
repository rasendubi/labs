public class Producer extends Thread {
    private final SynchronizedBuffer<Long> buffer;

    public Producer(SynchronizedBuffer<Long> buffer) {
        this.buffer = buffer;
    }

    @Override
    public void run() {
        for (int i = 0; i < 1000; ++i) {
            long number = stupidFib(i);
            try {
                buffer.write(number);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
            System.out.println(String.format("Written: %17s", number));
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
