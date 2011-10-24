package cspfj

class SolverIterator(val s: Solver) extends Iterator[Map[String, Int]] {

  var current = s.nextSolution();

  def hasNext = current != null;

  def next = if (hasNext) {
    val returned = current;
    current = s.nextSolution();
    returned
  } else {
    Iterator.empty.next;
  }

}