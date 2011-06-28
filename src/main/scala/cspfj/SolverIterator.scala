package cspfj

import scala.collection.Iterator
import scala.collection.JavaConversions

class SolverIterator(val s: Solver) extends Iterator[collection.mutable.Map[String, Int]] {

  var current = s.nextSolution();

  def hasNext = current != null;

  def next = if (hasNext) {
    val returned = current;
    current = s.nextSolution();
    JavaConversions.mapAsScalaMap(returned) map { e => e._1 -> e._2.toInt };
  } else {
    Iterator.empty.next;
  }

}