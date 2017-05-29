package concrete.runner

/**
  * Created by vion on 23/05/17.
  */
class Finisher(runtime: Thread, writer: ConcreteWriter) extends Thread {

  override def run(): Unit = {
    runtime.interrupt()

    while (runtime.isAlive) {
      Thread.sleep(10)
    }

    writer.disconnect()
  }
}
