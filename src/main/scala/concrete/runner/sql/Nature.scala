package concrete.runner.sql


sealed trait Nature


object Nature {
  def apply(nature: String): Nature = {
    nature.split(" ") match {
      case Array("satisfy") => Satisfy
      case Array("minimize", obj) => Minimize(obj)
      case Array("maximize", obj) => Maximize(obj)
    }
  }
}

case object Satisfy extends Nature

sealed abstract class Optimize(_variable: String) extends Nature {
  def variable: Seq[String] = _variable.split("\\|\\|").toSeq
}


case class Minimize(v: String) extends Optimize(v)

case class Maximize(v: String) extends Optimize(v)