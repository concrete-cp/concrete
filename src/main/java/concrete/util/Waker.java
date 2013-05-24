package concrete.util;

import java.util.TimerTask;

public final class Waker extends TimerTask {
    private final Thread thread;

    public Waker(final Thread thread) {
        this.thread = thread;
    }

    @Override
    public void run() {
        thread.interrupt();
    }

}
