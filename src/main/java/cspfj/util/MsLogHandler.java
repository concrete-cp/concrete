package cspfj.util;

import java.util.Formatter;
import java.util.Locale;
import java.util.logging.Handler;
import java.util.logging.LogRecord;

public class MsLogHandler extends Handler {

    final private static Formatter formatter = new Formatter(System.err,
            Locale.US);

    final private long start;

    public MsLogHandler(final long start) {
        super();
        this.start = start;
    }

    @Override
    public void close() throws SecurityException {
        System.out.close();
    }

    @Override
    public void flush() {
        System.out.flush();
    }

    @Override
    public void publish(final LogRecord arg0) {
        if (isLoggable(arg0)) {
            formatter.format("[%8.3f] %7s : %s (%s.%s)\n",
                    (arg0.getMillis() - start) / 1e3d, arg0.getLevel(), arg0
                            .getMessage(), arg0.getSourceClassName(), arg0
                            .getSourceMethodName());
        }
    }

}
