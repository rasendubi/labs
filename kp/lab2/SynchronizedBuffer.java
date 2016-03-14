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

        System.out.println(Thread.currentThread().getName() + " read " + value);
        System.out.print("Before: "); printBuffer();

        buffer[start] = null; // to allow GC
        start = nextIndex(start);
        --size;

        System.out.print("After:  "); printBuffer();
        System.out.println("");

        return value;
    }

    public synchronized void write(T value) throws InterruptedException {
        while (isFull()) {
            wait();
        }

        if (isEmpty()) {
            notifyAll();
        }

        System.out.println(Thread.currentThread().getName() + " write " + value);
        System.out.print("Before: "); printBuffer();

        buffer[end] = value;
        end = nextIndex(end);
        ++size;

        System.out.print("After:  "); printBuffer();
        System.out.println("");
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

    private void printBuffer() {
        System.out.print("[ ");
        int i = start;
        for (int j = 0; j < size; ++j) {
            System.out.print(String.format(" %11s", buffer[i]));
            i = nextIndex(i);
        }
        System.out.println(" ]");
    }
}
