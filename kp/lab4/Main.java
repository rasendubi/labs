import java.util.Random;
import java.util.concurrent.ForkJoinPool;

public class Main {
    private static final int SIZE = 100000000;
    private static final int BOUND = Integer.MAX_VALUE;
    private static final Random RANDOM = new Random();

    public static void main(String[] args) {
        int[] raw = new int[SIZE];
        for (int i = 0; i < SIZE; ++i) {
            raw[i] = RANDOM.nextInt();
        }

        ForkJoinPool forkJoinExecutor = new ForkJoinPool();

        long t = System.currentTimeMillis();
        System.out.println("Result: " + forkJoinExecutor.invoke(new MinMaxTask(raw)));
        System.out.println(System.currentTimeMillis() - t);
    }
}
