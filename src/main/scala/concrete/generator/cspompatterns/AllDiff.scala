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
import concrete.CSPOMDriver
import cspom.variable.SimpleExpression
import cspom.variable.CSPOMSeq

/**
 *  XCSP 2.0 uses a different case for alldifferent…
 */
object AllDifferent extends ConstraintCompilerNoData {
  def matchBool(c: CSPOMConstraint[_], p: CSPOM) = c.function == 'allDifferent
  def compile(c: CSPOMConstraint[_], p: CSPOM) = {
    val IntExpression.simpleSeq(args) = c.arguments
    replaceCtr(c, CSPOMDriver.allDifferent(args: _*) withParams c.params, p)
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

    if (c.function == 'alldifferent) {
      require(c.result == CSPOMConstant(true))
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
      delta ++= addCtr(CSPOMDriver.allDifferent(filt: _*), problem)
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
  type A = Set[CSPOMVariable[Int]]

  def DIFF_CONSTRAINT(constraint: CSPOMConstraint[_]): Option[Seq[CSPOMExpression[_]]] =
    PartialFunction.condOpt(constraint) {
      case CSPOMConstraint(
        CSPOMConstant(true),
        'sum,
        Seq(IntExpression.constSeq(coefs), CSPOMSeq(args), CSPOMConstant(constant: Int)),
        p) if (p.get("mode").contains("lt")) &&
        { coefs == Seq(-1, 1) || coefs == Seq(1, -1) } &&
        constant == 0 => args
    }
      .orElse(ALLDIFF_CONSTRAINT(constraint))

  //  (constraint.result.isTrue &&
  //    Set('gt, 'lt)(constraint.function)) ||
  //    (constraint.result.isFalse && Set('ge, 'le)(constraint.function)) ||
  //    ALLDIFF_CONSTRAINT(constraint)

  def ALLDIFF_CONSTRAINT(constraint: CSPOMConstraint[_]): Option[Seq[CSPOMExpression[_]]] = PartialFunction.condOpt(constraint) {
    case CSPOMConstraint(CSPOMConstant(true), 'alldifferent, args, _) => args
    case CSPOMConstraint(CSPOMConstant(true), 'sum,
      Seq(IntExpression.constSeq(coefs), CSPOMSeq(args), CSPOMConstant(constant: Int)),
      p) if p.get("mode").contains("ne") &&
      constant == 0 && (coefs == Seq(-1, 1) || coefs == Seq(1, -1)) => args
    case CSPOMConstraint(CSPOMConstant(false), 'eq, args, _) => args
  }

  val ITER = 750;

  val TABU_SIZE = 15;

  override def mtch(constraint: CSPOMConstraint[_], problem: CSPOM) = {
    DIFF_CONSTRAINT(constraint).flatMap { args =>
      val clique = expand(args.collect { case v: IntVariable => v }.toSet, problem)
      if (clique.size > args.size) {
        Some(clique)
      } else {
        None
      }
    }

  }

  /**
   * If constraint is part of a larger clique of inequalities, replace it by a
   * larger all-diff constraint.
   *
   * @param constraint
   */
  def compile(constraint: CSPOMConstraint[_], problem: CSPOM, clique: Set[CSPOMVariable[Int]]) = {

    val delta =
      if (ALLDIFF_CONSTRAINT(constraint).isDefined) {
        removeCtr(constraint, problem)
      } else {
        Delta()
      }

    delta ++ newAllDiff(clique.toSeq, problem)

  }

  //private val neighborsCache = new WeakHashMap[CSPOMVariable[_], Set[CSPOMVariable[_]]]

  def neighbors(v: CSPOMVariable[Int], problem: CSPOM,
                cache: WeakHashMap[CSPOMVariable[Int], Set[CSPOMVariable[Int]]]): Set[CSPOMVariable[Int]] = {

    cache.getOrElseUpdate(v,
      problem.deepConstraints(v).flatMap(DIFF_CONSTRAINT).flatten.collect {
        case v: IntVariable => v
      }.toSet - v)

  }

  /**
   * The pool contains all variables that can expand the base clique
   */
  private def populate(base: Set[CSPOMVariable[Int]], problem: CSPOM,
                       cache: WeakHashMap[CSPOMVariable[Int], Set[CSPOMVariable[Int]]]): Set[CSPOMVariable[Int]] = {
    val nb = base.iterator.map(neighbors(_, problem, cache))

    if (nb.isEmpty) {
      Set.empty
    } else {
      nb.reduceLeft(_ & _)
    }
  }

  private def expand(base: Set[CSPOMVariable[Int]], problem: CSPOM) = {

    val cache = new WeakHashMap[CSPOMVariable[Int], Set[CSPOMVariable[Int]]]

    var largest = base
    var clique = base

    var pool = populate(base, problem, cache)

    //final Set<CSPOMVariable[_]> base = new HashSet<CSPOMVariable[_]>(clique);

    var tabu: Map[CSPOMVariable[Int], Int] = Map.empty

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
  private def newAllDiff(scope: Seq[SimpleExpression[Int]], problem: CSPOM): Delta = {

    val allDiff = CSPOMDriver.allDifferent(scope: _*)

    val scopeSet = scope.toSet

    var delta = Delta()

    if (!scope.flatMap(problem.constraints).exists(c => isSubsumed(allDiff, c))) {
      logger.debug("New alldiff: " + allDiff.toString(new VariableNames(problem)))
      delta ++= addCtr(allDiff, problem)

      var removed = 0
      /* Remove newly subsumed neq/alldiff constraints. */

      for (
        v <- scope;
        c <- problem.constraints(v) if isSubsumed(c, allDiff)
      ) {
        removed += 1
        delta ++= removeCtr(c, problem)
      }
      logger.debug("removed " + removed + " constraints, " + problem.constraints.size + " left")
    }
    delta
  }

  private def isSubsumed(c: CSPOMConstraint[_], by: CSPOMConstraint[_]): Boolean = {
    (by ne c) && DIFF_CONSTRAINT(by).isDefined && ALLDIFF_CONSTRAINT(c).exists(_.forall(by.arguments.contains))
  }

  val RAND = new Random(0);

  private def pickTabu[B](pool: Iterable[B], tabu: Map[B, Int], iteration: Int) = {

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
