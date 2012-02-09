package cspfj.filter;

import java.util.Queue
import scala.annotation.tailrec
import scala.collection.IndexedSeq
import cspfj.constraint.Constraint
import cspfj.priorityqueues._
import cspfj.problem.Problem
import cspfj.problem.Variable
import cspfj.util.Loggable
import cspfj.Parameter
import cspfj.ParameterManager
import cspfj.Statistic
import cspfj.StatisticsManager
import cspfj.constraint.Removals
import cspfj.problem.EmptyDomainException

object AC3Constraint {
  @Parameter("ac.queue")
  var queueType: Class[_ <: Queue[Constraint]] = classOf[ScalaFifos[Constraint]]

  @Parameter("ac.key")
  var key = new Key[Constraint]() {
    def getKey(o: Constraint) = o.getEvaluation

    override def toString = "object.getEvaluation"
  };

  @Statistic
  var revisionCount = 0

  ParameterManager.register(this);

  def control(problem: Problem) = {

    for (c <- problem.constraints) {
      val sizes = c.scope map (_.dom.size)
      assert(c.consistentRevise(-1), c + " is inconsistent");
      assert(
        sizes.sameElements(c.scope map (_.dom.size)),
        c + " was revised!")
    }

    true;
  }
}

final class AC3Constraint(val problem: Problem, val queue: Queue[Constraint]) extends Filter with Loggable {
  @Statistic
  val substats = new StatisticsManager
  substats.register("ac.priorityQueue", queue);
  // private static final Logger LOGGER = Logger.getLogger(Filter.class
  // .getSimpleName());

  @Statistic
  var revisions = 0;

  def this(problem: Problem) =
    this(problem, AC3Constraint.queueType.getConstructor(classOf[Key[Constraint]]).newInstance(AC3Constraint.key))

  def reduceAll() = {
    AC3Constraint.revisionCount += 1;
    queue.clear();
    addAll();
    reduce();

  }

  def reduceFrom(modVar: Array[Int], modCons: Array[Int], cnt: Int) = {
    AC3Constraint.revisionCount += 1;
    queue.clear();
    // LOGGER.fine("reduce after " + cnt);
    for (v <- problem.variables) {
      if (modVar(v.getId) > cnt) {
        v.constraints.zipWithIndex.foreach {
          case (c, j) =>
            if (!c.isEntailed) {
              c match {
                case c: Removals =>
                  c.setRemovals(v.positionInConstraint(j),
                    AC3Constraint.revisionCount);
              }

              queue.offer(c);
            }
        }
      }
    }

    if (modCons != null) {
      problem.constraints.foreach { c =>
        if (modCons(c.getId) > cnt && !c.isEntailed) {
          c match {
            case c: Removals => c.fillRemovals(AC3Constraint.revisionCount)
          }

          queue.offer(c);
        }
      }
    }

    reduce();
  }

  def reduceAfter(variable: Variable) = {
    AC3Constraint.revisionCount += 1;
    if (variable == null) {
      true;
    } else {
      queue.clear();
      updateQueue(variable, null)
      //      variable.constraints.iterator.zipWithIndex.foreach {
      //        case (c, cp) =>
      //          enqueue(c, variable.positionInConstraint(cp))
      //      }
      //info("reduce " + AC3Constraint.revisionCount)
      reduce();
    }
  }

  private def enqueue(c: Constraint, modified: Int) {
    if (!c.isEntailed) {
      c.setRemovals(modified, AC3Constraint.revisionCount);
      queue.offer(c);
    }
  }

  private def updateQueue(modified: Variable, skip: Constraint) {
    val constraints = modified.constraints

    @tailrec
    def upd(i: Int) {
      if (i >= 0) {
        val c = constraints(i)

        if (c ne skip)
          enqueue(c, modified.positionInConstraint(i))

        upd(i - 1)
      }
    }

    upd(constraints.size - 1)
  }

  @tailrec
  private def updateQueue(sizes: Array[Int], constraint: Constraint, i: Int) {
    if (i >= 0) {
      val v = constraint.scope(i)

      if (v.dom.size != sizes(i))
        updateQueue(v, constraint)

      updateQueue(sizes, constraint, i - 1)
    }
  }

  @tailrec
  private def reduce(): Boolean = {
    if (queue.isEmpty) {
      assert(AC3Constraint.control(problem))
      true
    } else {
      val constraint = queue.poll();

      revisions += 1;
      val sizes = constraint.sizes

      if (constraint.consistentRevise(AC3Constraint.revisionCount)) {
        updateQueue(sizes, constraint, constraint.arity - 1)
        constraint.fillRemovals(-1);
        reduce()
      } else {
        constraint.weight += 1;
        false;
      }
    }
  }

  private def addAll() {
    problem.constraints.foreach { c =>
      if (!c.isEntailed) {
        c.fillRemovals(AC3Constraint.revisionCount);
        queue.offer(c);
      }
    }
  }

  override def toString = "AC-cons+" + queue.getClass().getSimpleName();

  def getStatistics = Map("revisions" -> revisions)

  def reduceAfter(constraints: Iterable[Constraint]) = {
    AC3Constraint.revisionCount += 1;
    queue.clear();

    for (c <- constraints if !c.isEntailed) {
      c.fillRemovals(AC3Constraint.revisionCount);
      queue.offer(c);
    }

    reduce();
  }

}
