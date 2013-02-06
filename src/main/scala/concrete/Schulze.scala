package concrete

import scala.Array.canBuildFrom

object Schulze extends App {

  val d1 = Array(
    Array(0, 2, 0, 1),
    Array(3, 0, 0, 2),
    Array(6, 3, 0, 2),
    Array(3, 2, 0, 0))

  val d2 = Array(
    Array(0, 0, 0, 0),
    Array(1, 0, 1, 0),
    Array(1, 0, 0, 0),
    Array(1, 0, 1, 0))

  val d = (d1, d2).zipped.map {
    case (l1, l2) => (l1, l2).zipped map { _ + _ }
  }

  println(d map (_.mkString(" ")) mkString ("\n"))
  println()

  val s = Table.schulze(d)

  val labels = Vector("BV", "Binary", "Binomial", "Fibo")
  
  println(Table.rank(s, s.indices).toList.sortBy(_._1) map {
    case (r, c) => "%d: %s".format(r, c.map(labels).mkString(" "))
  } mkString ("\n"))


}
