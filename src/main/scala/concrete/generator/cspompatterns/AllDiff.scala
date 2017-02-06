package concrete.generator.cspompatterns

import scala.collection.mutable.WeakHashMap
import scala.util.Random

import com.typesafe.scalalogging.LazyLogging

import concrete.CSPOMDriver
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.ConstraintCompiler
import cspom.compiler.Delta
import cspom.util.IntInterval
import cspom.util.RangeSet
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMVariable
import cspom.variable.IntVariable
import cspom.variable.SimpleExpression

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

  private val neCoefs1 = Seq(CSPOMConstant(-1), CSPOMConstant(1))
  private val neCoefs2 = Seq(CSPOMConstant(1), CSPOMConstant(-1))

  def DIFF_CONSTRAINT(constraint: CSPOMConstraint[_]): Option[Seq[CSPOMExpression[_]]] =
    ALLDIFF_CONSTRAINT(constraint).orElse {
      if (constraint.function == 'sum && constraint.result.isTrue && constraint.params.get("mode").contains("lt")) {
        val Seq(CSPOMSeq(coefs), CSPOMSeq(args), CSPOMConstant(constant: Int)) = constraint.arguments

        if (constant == 0 && (coefs == neCoefs1 || coefs == neCoefs2)) {
          Some(args)
        } else {
          None
        }
      } else None
    }

  //  (constraint.result.isTrue &&
  //    Set('gt, 'lt)(constraint.function)) ||
  //    (constraint.result.isFalse && Set('ge, 'le)(constraint.function)) ||
  //    ALLDIFF_CONSTRAINT(constraint)

  def ALLDIFF_CONSTRAINT(constraint: CSPOMConstraint[_]): Option[Seq[CSPOMExpression[_]]] = {
    if (constraint.function == 'alldifferent && constraint.result.isTrue) {
      Some(constraint.arguments)
    } else if (constraint.function == 'eq && constraint.result.isFalse) {
      Some(constraint.arguments)
    } else if (constraint.function == 'sum && constraint.result.isTrue && constraint.params.get("mode").contains("ne")) {
      val Seq(CSPOMSeq(coefs), CSPOMSeq(args), CSPOMConstant(constant: Int)) = constraint.arguments

      if (constant == 0 && (coefs == neCoefs1 || coefs == neCoefs2)) {
        Some(args)
      } else {
        None
      }
    } else None
  }
  //          
  //    case CSPOMConstraint(CSPOMConstant(true), 'alldifferent, args, _) => args
  //    case CSPOMConstraint(CSPOMConstant(true), 'sum,
  //      Seq(IntExpression.constSeq(coefs), CSPOMSeq(args), CSPOMConstant(constant: Int)),
  //      p) if p.get("mode").contains("ne") &&
  //      constant == 0 &&  => args
  //    case CSPOMConstraint(CSPOMConstant(false), 'eq, args, _) => args
  //  }

  val ITER = 750;

  val TABU_SIZE = 15;

  override def mtch(constraint: CSPOMConstraint[_], problem: CSPOM) = {
    DIFF_CONSTRAINT(constraint).flatMap { args =>
      // println(constraint.toString(problem.displayName))
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

    //    val vars = clique.toArray
    //
    //    val remove = for {
    //      i <- 0 until vars.length
    //      j <- i + 1 until vars.length
    //      c <- problem.deepConstraints(vars(i))
    //      if (ALLDIFF_CONSTRAINT(c) match {
    //        case Some(args) => args.length == 2 && args.contains(vars(j))
    //        case None => false
    //      })
    //
    //    } yield c
    //    
    //    println(s"removing ${.size} constraints")
    //
    //    removeCtr(remove, problem) ++ 
    //    
    newAllDiff(clique.toSeq, problem)

  }

  //private val neighborsCache = new WeakHashMap[CSPOMVariable[_], Set[CSPOMVariable[_]]]

  def neighbors(v: CSPOMVariable[Int], problem: CSPOM,
    cache: WeakHashMap[CSPOMVariable[Int], Set[CSPOMVariable[Int]]]): Set[CSPOMVariable[Int]] = {

    cache.getOrElseUpdate(v, {
      val s = Set.newBuilder[CSPOMVariable[Int]]
      for (c <- problem.deepConstraints(v); args <- DIFF_CONSTRAINT(c); a <- args) {
        a match {
          case v: IntVariable => s += v
          case _ =>
        }
      }
      s.result - v
    })
    //      problem.deepConstraints(v).flatMap(DIFF_CONSTRAINT).flatten.collect {
    //        case v: IntVariable => v
    //      }.toSet - v)

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

    //println(s"expanding ${base.map(_.toString(problem.displayName))}")

    val cache = new WeakHashMap[CSPOMVariable[Int], Set[CSPOMVariable[Int]]]

    var largest = base
    var clique = base

    var pool = populate(base, problem, cache)

    //final Set<CSPOMVariable[_]> base = new HashSet<CSPOMVariable[_]>(clique);

    var tabu: Map[CSPOMVariable[Int], Int] = Map.empty

    @annotation.tailrec
    def iterate(i: Int): Unit = if (i > 0) {
      val (newVar, newTabu) = pickTabu(pool, tabu, i);
      tabu = newTabu
      newVar match {
        case None => {
          /* Could not expand the clique, removing a variable (not from the base) */
          randPick((clique -- base).iterator) match {
            case Some(v) =>
              clique -= v
              pool = populate(clique, problem, cache)
              iterate(i - 1)
            case None =>
          }

        }
        case Some(variable) => {
          clique += variable
          if (clique.size > largest.size) {
            largest = clique
          }

          pool &= neighbors(variable, problem, cache)
          iterate(i - 1)
        }
      }
      //println(System.currentTimeMillis + " : " + clique.size)
    }

    iterate(ITER)

    //println(s"done: ${largest.size}")

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
      logger.debug("New alldiff: " + allDiff.toString(problem.displayName))
      delta ++= addCtr(allDiff, problem)

      var removed = 0
      /* Remove newly subsumed neq/alldiff constraints. */

      for (
        v <- scope;
        c <- problem.deepConstraints(v) if isSubsumed(c, allDiff)
      ) {
        removed += 1
        delta ++= removeCtr(c, problem)
      }
      logger.debug("removed " + removed + " constraints, " + problem.constraints.size + " left")
    }
    delta
  }

  private def isSubsumed(c: CSPOMConstraint[_], by: CSPOMConstraint[_]): Boolean = {
    (by ne c) && DIFF_CONSTRAINT(by).isDefined && {

      ALLDIFF_CONSTRAINT(c).exists { a =>
        a.forall(by.arguments.contains)
      }
    }
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
