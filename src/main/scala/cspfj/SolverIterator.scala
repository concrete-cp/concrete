package cspfj

class SolverIterator(val s: Solver) extends Iterator[Map[String, Int]] {

  var current = s.nextSolution();

  def hasNext = current.isDefined;

  def next = if (hasNext) {
    val returned = current;
    current = s.nextSolution();
    returned.get
  } else {
    Iterator.empty.next;
  }

}