package concrete.runner

import java.util.TimerTask

/**
  * Created by vion on 23/05/17.
  */
class Killer(runtime: Runtime) extends TimerTask {
  def run(): Unit = {
    runtime.exit(1)
  }
}
