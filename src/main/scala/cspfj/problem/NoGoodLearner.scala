package cspfj.problem;

import cspfj.constraint.extension.{ ExtensionConstraint2D, ExtensionConstraintGeneral }
import cspfj.constraint.extension.{ Matrix2D, TupleSet }
import cspfj.constraint.{ Constraint, DynamicConstraint }
import cspfj.heuristic.Pair
import cspfj.util.{ BitVector, Statistic }
import java.util.Deque
import scala.collection.JavaConversions

final class NoGoodLearner(private val problem: Problem, val learnMethod: LearnMethod) {

  @Statistic
  var nbNoGoods = 0;

  def noGoods(decisions: Deque[Pair]): Set[Constraint] = {
    if (LearnMethod.NONE.equals(learnMethod) || decisions.isEmpty()) {
      return Set.empty;
    }

    var modifiedConstraints: Set[Constraint] = Set.empty;

    var addedConstraints: List[Constraint] = Nil;

    var tuple: Vector[Int] = Vector.empty;

    var futureVariables = problem.variables.toSet;

    var currentScope: Vector[Variable] = Vector.empty;
    var level = 1;

    while (!decisions.isEmpty() && (level < problem.maxArity || learnMethod == LearnMethod.EXT)) {
      /*
             * Decisions are stacked, so the first decision in the search tree
             * is actually the last in the stack.
             */
      val lastDecision = decisions.pollLast();
      tuple :+= lastDecision.getIndex;
      currentScope :+= lastDecision.getVariable;
      futureVariables -= lastDecision.getVariable;

      for (fv <- futureVariables) {

        // logger.fine("checking " +
        // getVariable(levelVariables[level-1]));

        val changes = fv.dom.getAtLevel(level - 1).xor(fv.dom.getAtLevel(level));
        if (!changes.isEmpty) {

          val completeScope = currentScope :+ fv

          val constraint = learnConstraint(completeScope);

          if (constraint != null) {
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

    return modifiedConstraints;
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
      if (!changes.isEmpty()) {

        val scope = Seq(firstVariable, fv)
        val constraint = learnConstraint(scope);

        if (constraint != null) {

          val (base, varPos) = NoGoodLearner.makeBase(scope, tuple, constraint);

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
              addedConstraints ::= constraint;
            }
          }
        }
      }
    }

    if (!addedConstraints.isEmpty) {
      addedConstraints.foreach(problem.addConstraint)
    }

    return modifiedConstraints;
  }

  def learnConstraint(scope: Seq[Variable]): DynamicConstraint = {
    scope.head.dynamicConstraints.find(c => c.arity == scope.size &&
      scope.forall(c.scopeSet.contains)) match {
      case Some(c) => c
      case None => {
        learnMethod match {
          case LearnMethod.BIN => if (scope.size != 2) null else generateConstraint(scope);
          case LearnMethod.EXT => generateConstraint(scope);
          case _ => null;
        }

      }
    }
  }

  private def generateConstraint(scope: Seq[Variable]) = {
    if (scope.size == 2) {
      val matrix = new Matrix2D(scope(0).dom.maxSize, scope(1).dom.maxSize, true);
      new ExtensionConstraint2D(scope.toArray, matrix, false);
    } else {
      new ExtensionConstraintGeneral(new TupleSet(true), false, scope.toArray);
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

