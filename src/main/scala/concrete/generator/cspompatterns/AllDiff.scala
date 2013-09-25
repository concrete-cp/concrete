package concrete.generator.cspompatterns
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable
import scala.util.Random
import scala.util.control.Breaks
import cspom.Loggable
import cspom.compiler.ConstraintCompiler
import cspom.variable.CSPOMTrue
import cspom.compiler.Delta
import cspom.variable.CSPOMSeq

object AllDiff extends ConstraintCompiler with Loggable {
  type A = Set[CSPOMVariable]

  def DIFF_CONSTRAINT(constraint: CSPOMConstraint) =
    (constraint.result == CSPOMTrue) && Set("ne", "gt", "lt", "allDifferent").contains(constraint.function)

  def ALLDIFF_CONSTRAINT(constraint: CSPOMConstraint) =
    (constraint.result == CSPOMTrue) && "ne" == constraint.function ||
      "allDifferent" == constraint.function

  val ITER = 750;

  //val cliqueDetector = new CliqueDetector;

  def mtch(constraint: CSPOMConstraint, problem: CSPOM) = {
    val clique: Set[CSPOMVariable] = constraint match {
      case CSPOMConstraint(CSPOMTrue, func, args: Seq[CSPOMVariable], _) if Set("ne", "gt", "lt").contains(func) =>
        expand(args.toSet, problem)
      case CSPOMConstraint(CSPOMTrue, "allDifferent", Seq(CSPOMSeq(
        _, _, args: Seq[CSPOMVariable], _, _)), _) =>
        expand(args.toSet, problem)
      case _ => Set()
    }

    if (clique.size > constraint.scope.size) {
      Some(clique)
    } else {
      None
    }

  }

  /**
   * If constraint is part of a larger clique of inequalities, replace it by a
   * larger all-diff constraint.
   *
   * @param constraint
   */
  def compile(constraint: CSPOMConstraint, problem: CSPOM, clique: Set[CSPOMVariable]) = {

    problem.removeConstraint(constraint)
    newAllDiff(clique, problem);

    new Delta(constraint, constraint.scope ++ clique)

  }

  /**
   * The pool contains all variables that can expand the base clique
   */
  private def populate(base: Set[CSPOMVariable], problem: CSPOM) =
    base.iterator.map(problem.neighbors).reduceLeft((acc, vs) => acc & vs)

  private def expand(base: Set[CSPOMVariable], problem: CSPOM) = {

    var largest = base
    var clique = base

    var pool = populate(base, problem)

    //final Set<CSPOMVariable> base = new HashSet<CSPOMVariable>(clique);

    var tabu: Map[CSPOMVariable, Int] = Map.empty
    val mybreaks = new Breaks
    import mybreaks.{ break, breakable }

    breakable {
      for (i <- 1 to ITER) {
        val (newVar, newTabu) = AllDiff.pickTabu(pool, tabu, i);
        tabu = newTabu
        newVar match {
          case None => {
            /* Could not expand the clique, removing a variable (not from the base) */
            clique -= randPick((clique -- base).iterator).getOrElse(break)
            //            match {
            //              case None => break
            //              case Some(variable) => clique -= variable
            //            }
            pool = populate(clique, problem)
          }
          case Some(variable) => {
            clique += variable
            if (clique.size > largest.size) {
              largest = clique
            }

            pool &= problem.neighbors(variable)

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
  private def newAllDiff(scope: Set[CSPOMVariable], problem: CSPOM) {
    val allDiff = new CSPOMConstraint(CSPOMTrue,
      "allDifferent",
      scope.toList: _*);

    if (!problem.constraints.contains(allDiff)) {
      problem.addConstraint(allDiff);

      //      val constraints =
      //        scope.foldLeft(Set[CSPOMConstraint]())((acc, v) => acc ++ v.constraints) - allDiff;

      var removed = 0
      /* Remove newly subsumed neq/alldiff constraints. */
      for (
        v <- scope; c <- problem.constraints(v) if (
          c != allDiff && ALLDIFF_CONSTRAINT(c) && c.scope.forall(allDiff.scope.contains))
      ) {
        removed += 1
        problem.removeConstraint(c);
      }
      fine("removed " + removed + " constraints, " + problem.constraints.size + " left")
    }
  }

  val RAND = new Random(0);

  val TABU_SIZE = 15;

  private def pickTabu(pool: Iterable[CSPOMVariable], tabu: Map[CSPOMVariable, Int], iteration: Int) = {

    randPick(pool.iterator.filter(v =>
      tabu.get(v) match {
        case None => true
        case Some(i) => i < iteration
      })) match {
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
