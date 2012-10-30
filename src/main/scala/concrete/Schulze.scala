package concrete

import scala.Array.canBuildFrom

import concrete.Table

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

  Table.schulze(d, Vector("BV", "Binary", "Binomial", "Fibo"))

}