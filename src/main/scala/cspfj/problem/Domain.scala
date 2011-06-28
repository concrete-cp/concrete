package cspfj.problem;

trait Domain extends Iterable[Int] {
  def firstIndex: Int
  def lastIndex: Int
  def nextIndex(i: Int): Int
  def prevIndex(i: Int): Int
  def size: Int
  def index(value: Int): Int
  def value(index: Int): Int
  def maxSize: Int
  def containsIndex(index: Int): Boolean
  def setSingleIndex(index: Int)
  def removeIndex(index: Int)

  /**
   * @param lb
   * @return Removes all indexes starting from given lower bound.
   */
  def removeFromIndex(lb: Int)

  /**
   * @param ub
   * @return Removes all indexes up to given upper bound.
   */
  def removeToIndex(ub: Int)

  def setLevel(level: Int)

  def restoreLevel(level: Int)

  def getIndicesAtLevel(level: Int): Set[Int]

  def allValues: Seq[Int]

  /**
   * @param value
   * @return the index of the closest value lower or equal to the given value.
   */
  def indexClosestLeq(value: Int): Int

  /**
   * @param value
   * @return the index of the closest value greater or equal to the given
   *         value.
   */
  def indexClosestGeq(value: Int): Int

  def indices = new Iterator[Int] {
    private var current = firstIndex;

    def hasNext = current >= 0;

    def next = { val ret = current; current = nextIndex(current); ret }

    def remove = throw new UnsupportedOperationException
  }

}
