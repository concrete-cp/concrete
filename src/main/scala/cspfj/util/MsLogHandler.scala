package cspfj.util;

import java.util.Formatter;
import java.util.Locale;
import java.util.logging.Handler;
import java.util.logging.LogRecord;

final class MsLogHandler(val start: Long) extends Handler {
  def close() {
    System.out.close();
  }

  def flush() {
    System.out.flush();
  }

  def publish(arg0: LogRecord) {
    if (isLoggable(arg0)) {
      System.err.println("[%8.3f] %7s : %s (%s.%s)".format(
        (arg0.getMillis() - start) / 1e3d, arg0.getLevel(),
        arg0.getMessage(), arg0.getSourceClassName(),
        arg0.getSourceMethodName()));
    }
  }

}
