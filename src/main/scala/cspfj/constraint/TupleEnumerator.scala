package cspfj.constraint
import cspfj.constraint.extension.TupleManager

object TupleEnumerator {
  var checks = 0

  def clearStats() { checks = 0 }
}

trait TupleEnumerator extends Constraint {

  val tupleManager = new TupleManager(this, tuple)

  def findSupport(variablePosition: Int, index: Int): Boolean = {
    tupleManager.setFirstTuple(variablePosition, index);

    do {
      TupleEnumerator.checks += 1;
      if (check) {
        return true;
      }
    } while (tupleManager.setNextTuple(variablePosition));

    return false;
  }

  def getEvaluation = scope map (_.dom.size) product

  def simpleEvaluation = math.min(7, scope.count(_.dom.size > 1))

}