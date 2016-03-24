import java.util.concurrent.RecursiveTask;
import java.util.concurrent.ForkJoinTask;

public class MinMaxTask extends RecursiveTask<MinMaxTask.Result> {
    public static class Result {
        public final int min;
        public final int max;

        Result(int min, int max) {
            this.min = min;
            this.max = max;
        }

        @Override
        public String toString() {
            return "{ " + min + ", " + max + " }";
        }

        public static Result mappend(Result x, Result y) {
            return new Result(Math.min(x.min, y.min), Math.max(x.max, y.max));
        }

        public static Result MEMPTY = new Result(Integer.MAX_VALUE, Integer.MIN_VALUE);
    }

    private static final int THRESHOLD = 100;

    private final int[] data;
    private final int left;
    private final int right;

    public MinMaxTask(int[] data) {
        this(data, 0, data.length);
    }

    private MinMaxTask(int[] data, int left, int right) {
        this.data = data;
        this.left = left;
        this.right = right;
    }

    @Override
    protected Result compute() {
        int right = this.right;
        MinMaxTask other = null;

        if (right - left > THRESHOLD) {
            right = left + THRESHOLD;

            other = new MinMaxTask(data, right, this.right);
            other.fork();
        }

        Result x = Result.MEMPTY;
        for (int i = left; i < right; ++i) {
            x = Result.mappend(x, new Result(data[i], data[i]));
        }

        Result y = other != null ? other.join() : Result.MEMPTY;

        return Result.mappend(x, y);
    }
}
