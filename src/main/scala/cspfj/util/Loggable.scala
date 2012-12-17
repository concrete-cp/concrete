package cspfj.util

import java.util.logging.Logger
import java.util.logging.Level
trait Loggable {

  val logger: Logger = Logging.getLogger(this)

  //  def checkFormat(msg: String, refs: Seq[_]): String =
  //    if (!refs.isEmpty)
  //      checkFormat(msg.replaceFirst("{}", refs.head.toString), refs.tail);
  //    else msg

  //  def finer(msg: String, refs: Any*) = logger finer checkFormat(msg, refs)
  //
  //  def fine(msg: String, refs: Any*) = logger fine checkFormat(msg, refs)
  //
  //  def throwing(sourceClass: String, sourceMethod: String, t: Throwable) =
  //    logger throwing (sourceClass, sourceMethod, t)
  //
  //  def info(msg: String, refs: Any*) = logger info checkFormat(msg, refs)
  //
  //  def warning(msg: String, refs: Any*) = logger warning checkFormat(msg, refs)
  //
  //  def severe(msg: String, refs: Any*) = logger severe checkFormat(msg, refs)

  def logInfo = logger.isLoggable(Level.INFO)

  def logFine = logger.isLoggable(Level.FINE)

  final def setLevel(level: Level) {
    logger.setLevel(level)
    //logger.getHandlers()(0).setLevel(level)
  }
}

/**
 * Note: implementation taken from scalax.logging API
 */
object Logging {
  def loggerNameForClass(className: String) = {
    if (className endsWith "$") { className.substring(0, className.length - 1) }
    else { className }
  }

  def getLogger(logging: AnyRef) = Logger.getLogger(loggerNameForClass(logging.getClass.getName))

}
