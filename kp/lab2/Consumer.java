public class Consumer extends Thread {
    private final SynchronizedBuffer<Long> buffer;
    private final int delay;

    public Consumer(SynchronizedBuffer<Long> buffer, int delay) {
        this.buffer = buffer;
        this.delay = delay;
    }

    @Override
    public void run() {
        while (true) {
            try {
                long number = buffer.read();
                Thread.sleep(delay);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        }
    }
}
