public final class SynchronizedBuffer<T> {
    private final T[] buffer;

    private int start = 0;
    private int end = 0;
    private int size = 0;

    @SuppressWarnings("unchecked")
    public SynchronizedBuffer(int size) {
        buffer = (T[]) new Object[size];
    }

    public synchronized T read() throws InterruptedException {
        while (isEmpty()) {
            wait();
        }

        if (isFull()) {
            notifyAll();
        }

        T value = buffer[start];
        buffer[start] = null; // to allow GC
        start = nextIndex(start);
        --size;
        return value;
    }

    public synchronized void write(T value) throws InterruptedException {
        while (isFull()) {
            wait();
        }

        if (isEmpty()) {
            notifyAll();
        }

        buffer[end] = value;
        end = nextIndex(end);
        ++size;
    }

    private boolean isFull() {
        return size == buffer.length;
    }

    private boolean isEmpty() {
        return size == 0;
    }

    private int nextIndex(int n) {
        return (n + 1) % buffer.length;
    }
}
