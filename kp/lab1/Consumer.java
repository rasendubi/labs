public class Consumer extends Thread {
    private final SynchronizedBuffer<Long> buffer;

    public Consumer(SynchronizedBuffer<Long> buffer) {
        this.buffer = buffer;
    }

    @Override
    public void run() {
        while (true) {
            try {
                long number = buffer.read();
                System.out.println(String.format("Read:    %17s", number));
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        }
    }
}
