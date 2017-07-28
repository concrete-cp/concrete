package concrete.runner

/**
  * Created by vion on 23/05/17.
  */
class Finisher(runtime: Thread, writer: ConcreteWriter) extends Thread {


  override def run(): Unit = {

    runtime.interrupt()
    var tries = 1000
    while (runtime.isAlive && runtime.getState != Thread.State.BLOCKED) {

      Thread.sleep(10)
      tries -= 1
    }

    writer.disconnect()
  }
}
