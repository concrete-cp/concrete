package concrete.util

import java.util.concurrent.Executors

import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.util.{Failure, Try}

// original from https://gist.github.com/viktorklang/5409467
object CancellableFuture {
  def apply[T](work: => T)(implicit executor: ExecutionContext): CancellableFuture[T] = {
    new CancellableFuture(work)
  }
}

class CancellableFuture[T](work: => T)(implicit executor: ExecutionContext) extends Future[T] {
  private val p = Promise[T]()
  private val lock = new Object
  private var currentThread: Thread = null
  @volatile
  private var cancelled: Boolean = false

  val future = p.future

  run()

  private def run(): Unit = {
    p tryCompleteWith Future {
      throwCancellationExceptionIfCancelled {
        val thread = Thread.currentThread
        lock.synchronized {
          updateCurrentThread(thread)
        }
        try {
          throwCancellationExceptionIfCancelled(work)
        } finally {
          lock.synchronized {
            updateCurrentThread(null)
          } ne thread
          //Deal with interrupted flag of this thread in desired
        }
      }
    }
  }

  private def throwCancellationExceptionIfCancelled(body: => T) = {
    if (cancelled) throw new CancellationException
    body
  }

  private def updateCurrentThread(newThread: Thread): Thread = {
    val old = currentThread
    currentThread = newThread
    old
  }

  def cancel(): Unit = {
    lock.synchronized {
      Option(updateCurrentThread(null)).foreach(_.interrupt())
      cancelled |= p.tryFailure(new CancellationException)
    }
  }

  def isCancelled: Boolean = future.value match {
    case _@Some(Failure(t:CancellationException)) => true
    case _ => false
  }

  override def onComplete[U](f: (Try[T]) => U)(implicit executor: ExecutionContext): Unit = future.onComplete(f)

  override def isCompleted: Boolean = future.isCompleted

  override def value: Option[Try[T]] = future.value

  @throws[Exception](classOf[Exception])
  override def result(atMost: Duration)(implicit permit: CanAwait): T = future.result(atMost)

  @throws[InterruptedException](classOf[InterruptedException])
  @throws[TimeoutException](classOf[TimeoutException])
  override def ready(atMost: Duration)(implicit permit: CanAwait) = ???
  
  def transform[S](f: Try[T] => Try[S])(implicit executor: ExecutionContext): Future[S] = ???
  def transformWith[S](f: Try[T] => Future[S])(implicit executor: ExecutionContext): Future[S] = ???
//  CancellableFutureImpl.this.type = {
//    new CancellableFutureImpl(Await.result(future, atMost))
//  }
}

object CancellableFutureTestApp extends App {
  def blockCall(name: String, sec: Int) = {
    println(s"$name: start")
    try {
      for {i <- 1 to sec} {
        Thread.sleep(1000)
        println(s"$name: i'm alive $i")
      }
      println(s"$name finish")
    } catch {
      case _: InterruptedException =>
        println(s"$name stop")
    }
  }

  val pool = Executors.newFixedThreadPool(1)
  implicit val ctx = ExecutionContext.fromExecutor(pool)

  val cancellableFutures = for {i <- 1 to 200} yield {
    CancellableFuture(blockCall(s"task-$i", 5))
  }

  Thread.sleep(3000)

  for {
    (f, i) <- cancellableFutures.zipWithIndex
    if i < 100
  } f.cancel()

  Thread.sleep(6000)

  cancellableFutures.foreach(_.cancel())

  println(s"${cancellableFutures.count(_.isCancelled)} tasks was cancelled")

  pool.shutdown()
}
