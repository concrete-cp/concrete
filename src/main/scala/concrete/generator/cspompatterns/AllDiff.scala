package concrete.generator.cspompatterns
import cspom.CSPOM
import cspom.constraint.CSPOMConstraint
import cspom.constraint.GeneralConstraint
import cspom.variable.CSPOMVariable
import scala.util.Random
import scala.util.control.Breaks
import cspom.Loggable
import cspom.compiler.ConstraintCompiler

final class AllDiff(val problem: CSPOM) extends ConstraintCompiler with Loggable {

  def DIFF_CONSTRAINT(constraint: CSPOMConstraint) =
    constraint.isInstanceOf[GeneralConstraint] &&
      Set("ne", "gt", "lt", "allDifferent").contains(constraint.description)

  def ALLDIFF_CONSTRAINT(constraint: CSPOMConstraint) =
    constraint.isInstanceOf[GeneralConstraint] &&
      "ne" == constraint.description ||
      "allDifferent" == constraint.description

  val ITER = 750;

  //val cliqueDetector = new CliqueDetector;

  /**
   * If constraint is part of a larger clique of inequalities, replace it by a
   * larger all-diff constraint.
   *
   * @param constraint
   */
  def alldiff(constraint: CSPOMConstraint) = {
    if (!DIFF_CONSTRAINT(constraint)) false
    else {
      // System.out.print(constraint);

      //val clique: Set[CSPOMVariable] = Set.empty ++ constraint.scope

      // val pool = populate(constraint.scope);
      //print(constraint + " : ")

      val clique = expand(constraint.scope.toSet);
      //println(clique.size)
      if (clique.size > constraint.scope.size) {
        problem.removeConstraint(constraint)
        newAllDiff(clique);
        true
      } else false
    }

  }

  /**
   * Lazy neighbors computation
   */
  private class Neighbors {
    var neighbors: Map[CSPOMVariable, Set[CSPOMVariable]] = Map.empty

    def get(v: CSPOMVariable) = neighbors.get(v) match {
      case Some(set) => set
      case None => {
        val n = (v.constraints.filter(DIFF_CONSTRAINT).foldLeft(Set[CSPOMVariable]())(
          (acc, c) => acc ++ c.scope) - v)
        neighbors += v -> n
        n
      }
    }

  }

  /**
   * The pool contains all variables that can expand the base clique
   */
  private def populate(base: Set[CSPOMVariable], neighbors: Neighbors) =
    base.iterator.map(neighbors.get).reduceLeft((acc, vs) => acc & vs)

  private def expand(base: Set[CSPOMVariable]) = {

    var largest = base
    var clique = base

    val neighbors = new Neighbors

    var pool = populate(base, neighbors)

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
            AllDiff.randPick((clique -- base).iterator) match {
              case None => break
              case Some(variable) => clique -= variable
            }
            pool = populate(clique, neighbors)
          }
          case Some(variable) => {
            clique += variable
            if (clique.size > largest.size) {
              largest = clique
            }

            pool &= neighbors.get(variable)

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
  private def newAllDiff(scope: Set[CSPOMVariable]) {
    val allDiff = new GeneralConstraint(
      "allDifferent",
      scope.toList: _*);

    if (!problem.constraints.contains(allDiff)) {
      problem.addConstraint(allDiff);

      //      val constraints =
      //        scope.foldLeft(Set[CSPOMConstraint]())((acc, v) => acc ++ v.constraints) - allDiff;

      var removed = 0
      /* Remove newly subsumed neq/alldiff constraints. */
      for (
        v <- scope; c <- v.constraints if (c != allDiff && ALLDIFF_CONSTRAINT(c) && c.scope.forall(allDiff.scopeSet.contains))
      ) {
        removed += 1
        problem.removeConstraint(c);
      }
      fine("removed " + removed + " constraints, " + problem.constraints.size + " left")
    }
  }

  /**
   * If the given constraint is an all-different or neq constraint, remove it
   * if it is subsumed by another difference constraint.
   *
   * @param constraint
   */
  def dropSubsumedDiff(constraint: CSPOMConstraint) = {
    if (ALLDIFF_CONSTRAINT(constraint) &&
      AllDiff.haveSubsumingConstraint(constraint, DIFF_CONSTRAINT)) {

      problem.removeConstraint(constraint);
      //fine("subsumed ! " + problem.constraints.size + " remaining")
      true;
    } else {
      false;
    }
  }

  override def compile(constraint: CSPOMConstraint) = {
    dropSubsumedDiff(constraint) || alldiff(constraint)
  }

}

object AllDiff {
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

  private def haveSubsumingConstraint(
    constraint: CSPOMConstraint, validator: CSPOMConstraint => Boolean) =
    constraint.scope.minBy(_.constraints.size).constraints.exists(
      c => c != constraint && validator(c) && constraint.scope.forall(c.scopeSet.contains))

}
