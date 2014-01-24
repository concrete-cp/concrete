package concrete.generator.cspompatterns
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable
import scala.util.Random
import scala.util.control.Breaks._
import cspom.Loggable
import cspom.compiler.ConstraintCompiler
import cspom.variable.CSPOMTrue
import cspom.compiler.Delta
import cspom.variable.CSPOMSeq
import scala.collection.mutable.WeakHashMap
import cspom.variable.CSPOMExpression

object AllDiff extends ConstraintCompiler with Loggable {
  type A = Set[CSPOMExpression]

  def DIFF_CONSTRAINT(constraint: CSPOMConstraint) =
    (constraint.result == CSPOMTrue) && Set('ne, 'gt, 'lt, 'allDifferent)(constraint.function)

  def ALLDIFF_CONSTRAINT(constraint: CSPOMConstraint) =
    (constraint.result == CSPOMTrue) && 'ne == constraint.function ||
      'allDifferent == constraint.function

  val ITER = 750;

  val TABU_SIZE = 15;

  override def mtch(constraint: CSPOMConstraint, problem: CSPOM) = {

    constraint match {
      case CSPOMConstraint(CSPOMTrue, func, args: Seq[CSPOMExpression], _) if Set('allDifferent, 'ne, 'gt, 'lt)(func) =>
        val clique = expand(args.toSet, problem)
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
  def compile(constraint: CSPOMConstraint, problem: CSPOM, clique: Set[CSPOMExpression]) = {

    problem.removeConstraint(constraint)

    Delta().removed(constraint) ++ newAllDiff(clique.toSeq, problem)

  }

  //private val neighborsCache = new WeakHashMap[CSPOMVariable, Set[CSPOMVariable]]

  def neighbors(v: CSPOMExpression, problem: CSPOM,
    cache: WeakHashMap[CSPOMVariable, Set[CSPOMExpression]]): Set[CSPOMExpression] = {
    v match {
      case v: CSPOMVariable => cache.getOrElseUpdate(v,
        problem.constraints(v).filter(DIFF_CONSTRAINT).flatMap(_.arguments).toSet - v)
      case e: CSPOMExpression => Set()
      //case _ => throw new UnsupportedOperationException
    }

  }

  /**
   * The pool contains all variables that can expand the base clique
   */
  private def populate(base: Set[CSPOMExpression], problem: CSPOM,
    cache: WeakHashMap[CSPOMVariable, Set[CSPOMExpression]]) =
    base.iterator.map(neighbors(_, problem, cache)).reduceLeft(_ & _)

  private def expand(base: Set[CSPOMExpression], problem: CSPOM) = {

    val cache = new WeakHashMap[CSPOMVariable, Set[CSPOMExpression]]

    var largest = base
    var clique = base

    var pool = populate(base, problem, cache)

    //final Set<CSPOMVariable> base = new HashSet<CSPOMVariable>(clique);

    var tabu: Map[CSPOMExpression, Int] = Map.empty

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
  private def newAllDiff(scope: Seq[CSPOMExpression], problem: CSPOM): Delta = {
    val allDiff = new CSPOMConstraint(CSPOMTrue, 'allDifferent, scope: _*);

    var delta = Delta()

    if (!problem.constraints.contains(allDiff)) {
      problem.ctr(allDiff);

      delta = delta.added(allDiff)
      //      val constraints =
      //        scope.foldLeft(Set[CSPOMConstraint]())((acc, v) => acc ++ v.constraints) - allDiff;

      var removed = 0
      /* Remove newly subsumed neq/alldiff constraints. */

      scope.iterator.collect {
        case v: CSPOMVariable => v
      } flatMap (problem.constraints) filter {
        c => (c ne allDiff) && ALLDIFF_CONSTRAINT(c) && c.scope.forall(allDiff.scope.contains)
      } foreach {
        c =>
          removed += 1
          problem.removeConstraint(c);
          delta = delta.removed(c)
      }
      fine("removed " + removed + " constraints, " + problem.constraints.size + " left")
    }
    delta
  }

  val RAND = new Random(0);

  private def pickTabu(pool: Iterable[CSPOMExpression], tabu: Map[CSPOMExpression, Int], iteration: Int) = {

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

}
