package concrete.util;

import java.util.Formatter
import java.util.Locale
import java.util.logging.Handler
import java.util.logging.LogRecord;
import java.io.PrintStream

final class MsLogHandler(val start: Long, val out: PrintStream = System.err) extends Handler {
  def close() {
    out.close();
  }

  def flush() {
    out.flush();
  }

  def publish(arg0: LogRecord) {
    if (isLoggable(arg0)) {
      out.println("[%8.3f] %7s : %s (%s.%s)".format(
        (arg0.getMillis() - start) / 1e3d, arg0.getLevel(),
        arg0.getMessage(), arg0.getSourceClassName(),
        arg0.getSourceMethodName()));
    }
  }

}
