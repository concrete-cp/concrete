package concrete.generator.cspompatterns

import scala.collection.mutable.WeakHashMap
import scala.util.Random
import scala.util.control.Breaks.break
import scala.util.control.Breaks.breakable
import com.typesafe.scalalogging.LazyLogging
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.VariableNames
import cspom.compiler.ConstraintCompiler
import cspom.compiler.Delta
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMVariable
import cspom.compiler.VariableCompiler
import cspom.variable.CSPOMConstant
import cspom.variable.IntVariable
import cspom.util.RangeSet
import cspom.util.Interval
import cspom.util.IntInterval
import cspom.variable.IntExpression
import cspom.compiler.ConstraintCompilerNoData

/**
 *  XCSP 2.0 uses a different case for alldifferent…
 */
object AllDifferent extends ConstraintCompilerNoData {
  def matchBool(c: CSPOMConstraint[_], p: CSPOM) = c.function == 'allDifferent
  def compile(c: CSPOMConstraint[_], p: CSPOM) = {
    replaceCtr(c, CSPOMConstraint('alldifferent, c.arguments, c.params), p)
  }
  def selfPropagation = false
}

/**
 * Removes constants from alldifferent constraints
 */
object AllDiffConstant extends ConstraintCompiler {
  type A = (Seq[Int])

  def selfPropagation = true

  override def mtch(c: CSPOMConstraint[_], p: CSPOM): Option[A] = {
    if (AllDiff.ALLDIFF_CONSTRAINT(c)) {
      val constants = c.arguments.collect {
        case CSPOMConstant(k: Int) => k
      }
      if (constants.isEmpty) {
        None
      } else {
        Some(constants)
      }
    } else {
      None
    }

  }

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM, constants: A) = {
    val variables = constraint.arguments.collect {
      case v: IntVariable => v
    }
    require(constants.distinct.size + variables.size == constraint.arguments.size)

    val constantSet = RangeSet(constants.map(k => IntInterval.singleton(k)))

    val filt = variables.map { v: IntVariable =>
      applyDomain(v, v.domain -- constantSet)
      //if (r == v.domain) v else IntVariable(r)
    }

    var delta = removeCtr(constraint, problem)

    if (filt.length > 1) {
      delta ++= addCtr(CSPOMConstraint('alldifferent, filt), problem)
    }

    for ((v, f) <- (variables, filt).zipped) {
      delta ++= replace(v, f, problem)
    }

    delta
  }
}

/**
 * Aggregates cliques of alldifferent constraints. Uses a small tabu search engine
 * to detect max-cliques.
 */
object AllDiff extends ConstraintCompiler with LazyLogging {
  type A = Set[CSPOMVariable[_]]

  def DIFF_CONSTRAINT(constraint: CSPOMConstraint[_]) =
    (constraint.result.isTrue &&
      Set('gt, 'lt, 'alldifferent)(constraint.function)) ||
      (constraint.result.isFalse && Set('eq, 'ge, 'le)(constraint.function))

  def ALLDIFF_CONSTRAINT(constraint: CSPOMConstraint[_]) =
    (constraint.result.isFalse) && 'eq == constraint.function ||
      'alldifferent == constraint.function

  val ITER = 750;

  val TABU_SIZE = 15;

  override def mtch(constraint: CSPOMConstraint[_], problem: CSPOM) = {

    constraint match {
      case c @ CSPOMConstraint(_, func, args: Seq[_], _) if DIFF_CONSTRAINT(c) =>
        val clique = expand(args.collect { case v: CSPOMVariable[_] => v }.toSet, problem)
        if (clique.size > constraint.arguments.size) {
          Some(clique)
        } else {
          None
        }
      case _ => None
    }

  }

  /**
   * If constraint is part of a larger clique of inequalities, replace it by a
   * larger all-diff constraint.
   *
   * @param constraint
   */
  def compile(constraint: CSPOMConstraint[_], problem: CSPOM, clique: Set[CSPOMVariable[_]]) = {

    val delta =
      if (ALLDIFF_CONSTRAINT(constraint)) {
        problem.removeConstraint(constraint)
        Delta().removed(constraint)
      } else {
        Delta()
      }

    delta ++ newAllDiff(clique.toSeq, problem)

  }

  //private val neighborsCache = new WeakHashMap[CSPOMVariable[_], Set[CSPOMVariable[_]]]

  def neighbors(v: CSPOMVariable[_], problem: CSPOM,
                cache: WeakHashMap[CSPOMVariable[_], Set[CSPOMVariable[_]]]): Set[CSPOMVariable[_]] = {

    cache.getOrElseUpdate(v,
      problem.constraints(v).filter(DIFF_CONSTRAINT).flatMap(_.arguments).collect {
        case v: CSPOMVariable[_] => v
      }.toSet - v)

  }

  /**
   * The pool contains all variables that can expand the base clique
   */
  private def populate(base: Set[CSPOMVariable[_]], problem: CSPOM,
                       cache: WeakHashMap[CSPOMVariable[_], Set[CSPOMVariable[_]]]): Set[CSPOMVariable[_]] = {
    val nb = base.iterator.map(neighbors(_, problem, cache))

    if (nb.isEmpty) {
      Set()
    } else {
      nb.reduceLeft(_ & _)
    }
  }

  private def expand(base: Set[CSPOMVariable[_]], problem: CSPOM) = {

    val cache = new WeakHashMap[CSPOMVariable[_], Set[CSPOMVariable[_]]]

    var largest = base
    var clique = base

    var pool = populate(base, problem, cache)

    //final Set<CSPOMVariable[_]> base = new HashSet<CSPOMVariable[_]>(clique);

    var tabu: Map[CSPOMVariable[_], Int] = Map.empty

    breakable {
      for (i <- 1 to ITER) {
        val (newVar, newTabu) = pickTabu(pool, tabu, i);
        tabu = newTabu
        newVar match {
          case None => {
            /* Could not expand the clique, removing a variable (not from the base) */
            clique -= randPick((clique -- base).iterator).getOrElse(
              /*BREAK*/ break /*BREAK*/ )
            pool = populate(clique, problem, cache)
          }
          case Some(variable) => {
            clique += variable
            if (clique.size > largest.size) {
              largest = clique
            }

            pool &= neighbors(variable, problem, cache)

          }
        }
        //println(System.currentTimeMillis + " : " + clique.size)
      }
    }

    largest
  }

  /**
   * Adds a new all-diff constraint of the specified scope. Any newly subsumed
   * neq/all-diff constraints are removed.
   *
   * @param scope
   */
  private def newAllDiff(scope: Seq[CSPOMExpression[_]], problem: CSPOM): Delta = {

    val allDiff = CSPOMConstraint('alldifferent, scope);

    val scopeSet = scope.toSet

    var delta = Delta()

    if (!scope.flatMap(problem.constraints).exists(c => isSubsumed(allDiff, c))) {
      problem.ctr(allDiff);
      logger.debug("New alldiff: " + allDiff.toString(new VariableNames(problem)))

      delta = delta.added(allDiff)
      //      val constraints =
      //        scope.foldLeft(Set[CSPOMConstraint]())((acc, v) => acc ++ v.constraints) - allDiff;

      var removed = 0
      /* Remove newly subsumed neq/alldiff constraints. */

      scope.iterator.flatMap(problem.constraints).filter(c => isSubsumed(c, allDiff)).foreach {
        c =>
          removed += 1
          problem.removeConstraint(c);
          delta = delta.removed(c)
      }
      logger.debug("removed " + removed + " constraints, " + problem.constraints.size + " left")
    }
    delta
  }

  private def isSubsumed(c: CSPOMConstraint[_], by: CSPOMConstraint[_]): Boolean = {
    (by ne c) && DIFF_CONSTRAINT(by) && ALLDIFF_CONSTRAINT(c) && c.arguments.forall(by.arguments.contains)
  }

  val RAND = new Random(0);

  private def pickTabu[A](pool: Iterable[A], tabu: Map[A, Int], iteration: Int) = {

    randPick(pool.iterator.filter(v =>
      tabu.get(v).forall(_ < iteration))) match {
      case None =>
        (None, tabu)
      case Some(v) =>
        (Some(v), tabu + (v -> (iteration + TABU_SIZE)))
    }

  }

  private def randPick[T](it: Iterator[T]): Option[T] = {
    var tie = 1
    var returned: Option[T] = None
    for (i <- it) {
      if (RAND.nextDouble() * tie < 1) returned = Some(i)
      tie += 1
    }
    returned
  }
  def selfPropagation = false
}
