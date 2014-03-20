package concrete.generator.cspompatterns
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.IntVariable
import scala.util.Random
import scala.util.control.Breaks._
import com.typesafe.scalalogging.slf4j.LazyLogging
import cspom.compiler.ConstraintCompiler
import cspom.variable.CSPOMConstant
import cspom.compiler.Delta
import cspom.variable.CSPOMSeq
import scala.collection.mutable.WeakHashMap
import cspom.variable.CSPOMExpression
import cspom.VariableNames

object AllDiff extends ConstraintCompiler with LazyLogging {
  type A = Set[IntVariable]

  def DIFF_CONSTRAINT(constraint: CSPOMConstraint[_]) =
    (constraint.result.isTrue) &&
      Set('ne, 'gt, 'lt, 'allDifferent)(constraint.function)

  def ALLDIFF_CONSTRAINT(constraint: CSPOMConstraint[_]) =
    (constraint.result.isTrue) && 'ne == constraint.function ||
      'allDifferent == constraint.function

  val ITER = 750;

  val TABU_SIZE = 15;

  override def mtch(constraint: CSPOMConstraint[_], problem: CSPOM) = {

    constraint match {
      case c @ CSPOMConstraint(_, func, args: Seq[CSPOMExpression[Int]], _) if DIFF_CONSTRAINT(c) =>
        val clique = expand(args.collect { case v: IntVariable => v }.toSet, problem)
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
  def compile(constraint: CSPOMConstraint[_], problem: CSPOM, clique: Set[IntVariable]) = {

    val delta =
      if (ALLDIFF_CONSTRAINT(constraint)) {
        problem.removeConstraint(constraint)
        Delta().removed(constraint)
      } else {
        Delta()
      }

    delta ++ newAllDiff(clique.toSeq, problem)

  }

  //private val neighborsCache = new WeakHashMap[IntVariable, Set[IntVariable]]

  def neighbors(v: IntVariable, problem: CSPOM,
    cache: WeakHashMap[IntVariable, Set[IntVariable]]): Set[IntVariable] = {

    cache.getOrElseUpdate(v,
      problem.constraints(v).filter(DIFF_CONSTRAINT).flatMap(_.arguments).collect {
        case v: IntVariable => v
      }.toSet - v)

  }

  /**
   * The pool contains all variables that can expand the base clique
   */
  private def populate(base: Set[IntVariable], problem: CSPOM,
    cache: WeakHashMap[IntVariable, Set[IntVariable]]): Set[IntVariable] = {
    val nb = base.iterator.map(neighbors(_, problem, cache))

    if (nb.isEmpty) {
      Set()
    } else {
      nb.reduceLeft(_ & _)
    }
  }

  private def expand(base: Set[IntVariable], problem: CSPOM) = {

    val cache = new WeakHashMap[IntVariable, Set[IntVariable]]

    var largest = base
    var clique = base

    var pool = populate(base, problem, cache)

    //final Set<IntVariable> base = new HashSet<IntVariable>(clique);

    var tabu: Map[IntVariable, Int] = Map.empty

    breakable {
      for (i <- 1 to ITER) {
        val (newVar, newTabu) = pickTabu(pool, tabu, i);
        tabu = newTabu
        newVar match {
          case None => {
            /* Could not expand the clique, removing a variable (not from the base) */
            clique -= randPick((clique -- base).iterator).getOrElse( /*BREAK*/ break /*BREAK*/ )
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
  private def newAllDiff(scope: Seq[CSPOMExpression[Int]], problem: CSPOM): Delta = {

    val allDiff = CSPOMConstraint('allDifferent, scope);

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
