package concrete

import concrete.constraint.extension.ExtensionConstraint
import concrete.constraint.extension.BinaryExt
import concrete.constraint.extension.ExtensionConstraintGeneral
import concrete.constraint.extension.Matrix2D
import concrete.constraint.Constraint
import concrete.util.BitVectorIterator
import concrete.constraint.extension.TupleTrieSet
import cspom.Statistic;

sealed trait LearnMethod

object LearnMethod {
  def apply(s: String) = s match {
    case "BIN" => LearnBin
    case "NONE" => NoLearn
    case "EXT" => LearnExt
    case "CONSERVATIVE" => LearnConservative
  }
}

case object NoLearn extends LearnMethod
case object LearnExt extends LearnMethod
case object LearnBin extends LearnMethod
case object LearnConservative extends LearnMethod

final class NoGoodLearner(private val problem: Problem, val learnMethod: LearnMethod) {

  @Statistic
  var nbNoGoods = 0;

  def noGoods(decisions: List[Pair]): Set[Constraint] =
    if (learnMethod == NoLearn || decisions == Nil) {
      Set.empty;
    } else {

      var modifiedConstraints: Set[Constraint] = Set.empty;

      var addedConstraints: List[Constraint] = Nil;

      var tuple: Vector[Int] = Vector.empty;

      var futureVariables = problem.variables.toSet;

      var currentScope: Vector[Variable] = Vector.empty;
      var level = 1;

      while (decisions != Nil && (level < problem.maxArity || learnMethod == LearnExt)) {
        /**
         * Decisions are stacked, so the first decision in the search tree
         * is actually the last in the stack.
         */
        val lastDecision = decisions.last;
        tuple :+= lastDecision.index;
        currentScope :+= lastDecision.variable;
        futureVariables -= lastDecision.variable;

        for (fv <- futureVariables) {

          // logger.fine("checking " +
          // getVariable(levelVariables[level-1]));

          val changes = fv.dom.getAtLevel(level - 1).xor(fv.dom.getAtLevel(level));
          if (!changes.isEmpty) {

            val completeScope = currentScope :+ fv

            for (constraint <- learnConstraint(completeScope)) {
              val (base, varPos) = NoGoodLearner.makeBase(completeScope, tuple, constraint);

              var newNogoods = 0;
              var i = changes.nextSetBit(0);
              while (i >= 0) {
                base(varPos) = i;
                newNogoods += constraint.removeTuples(base);
                i = changes.nextSetBit(i + 1)
              }
              if (newNogoods > 0) {
                nbNoGoods += newNogoods;
                modifiedConstraints += constraint;
                if (constraint.getId > problem.maxCId) {
                  // LOGGER.info("Added " + constraint);
                  addedConstraints ::= constraint;
                }
              }
            }

          }
        }
        level += 1;
      }

      if (!addedConstraints.isEmpty) {
        addedConstraints.foreach(problem.addConstraint)
        // LOGGER.info(problem.getNbConstraints() + " constraints");
      }

      modifiedConstraints;
    }

  def binNoGoods(firstVariable: Variable): Set[Constraint] = {
    val tuple = List(firstVariable.dom.first)
    var modifiedConstraints: Set[Constraint] = Set.empty;

    var addedConstraints: List[Constraint] = Nil;

    for (
      fv <- problem.variables;
      if fv != firstVariable
    ) {
      // logger.fine("checking " +
      // getVariable(levelVariables[level-1]));

      val changes = fv.dom.getAtLevel(0).xor(fv.dom.getAtLevel(1));
      if (!changes.isEmpty) {

        val scope = Seq(firstVariable, fv)
        for (constraint <- learnConstraint(scope)) {

          val (base, varPos) = NoGoodLearner.makeBase(scope, tuple, constraint);

          val newNogoods = new BitVectorIterator(changes) map { i =>
            base(varPos) = i
            constraint.removeTuples(base)
          } sum

          if (newNogoods > 0) {
            nbNoGoods += newNogoods;
            modifiedConstraints += constraint;
            if (constraint.getId > problem.maxCId) {
              addedConstraints ::= constraint;
            }
          }

        }
      }
    }

    addedConstraints.foreach(problem.addConstraint)

    modifiedConstraints;
  }

  def learnConstraint(scope: Seq[Variable]): Option[ExtensionConstraint] = {
    scope.head.dynamicConstraints.find(c => c.arity == scope.size &&
      scope.forall(c.scopeSet.contains)) match {
      case Some(c) => Some(c)
      case None => learnMethod match {
        case LearnBin => if (scope.size != 2) None else Some(generateConstraint(scope));
        case LearnExt => Some(generateConstraint(scope));
        case _ => None;
      }

    }
  }

  private def generateConstraint(scope: Seq[Variable]) = {
    if (scope.size == 2) {
      val matrix = new Matrix2D(scope(0).dom.maxSize, scope(1).dom.maxSize, true);
      BinaryExt(scope.toArray, matrix, false);
    } else {
      new ExtensionConstraintGeneral(new TupleTrieSet(true), false, scope.toArray);
    }
  }
}

object NoGoodLearner {

  /**
   * Sets the base array given as a parameter so that the values of base
   * correspond to the values of the values array reordered such that they
   * correspond to the variables of the scope of the constraint. Variables
   * present in the scope of the constraint but not in the scope sequence
   * result in a 0 value in the base array. Last variable of scope is
   * ignored. Returns the position of the last variable of scope[] in the
   * constraint's scope.
   *
   * @param scope
   * @param values
   * @param constraint
   * @param base
   * @return
   */
  def makeBase(scope: Seq[Variable], values: Seq[Int], constraint: Constraint) = {
    assert(scope.size == values.size + 1);
    assert(scope.length == constraint.arity);

    val base = new Array[Int](constraint.arity)

    scope.init.zipWithIndex.foreach { z =>
      base(constraint.position(z._1)) = values(z._2)
    }

    (base, constraint.position(scope.last));
  }
}

