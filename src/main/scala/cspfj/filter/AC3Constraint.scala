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
import cspfj.heuristic.revision.Eval

object AC3Constraint {
  @Parameter("ac3c.queue")
  var queueType: Class[_ <: Queue[Constraint]] = classOf[BinomialHeap[Constraint]]

  @Parameter("ac3c.key")
  var keyType: Class[_ <: Key[Constraint]] = classOf[Eval]

  ParameterManager.register(this);

  def control(problem: Problem) = {

    for (c <- problem.constraints) {
      c.fillRemovals()
      val sizes = c.scope map (_.dom.size)
      assert(c.consistentRevise(), c + " is inconsistent");
      assert(
        sizes.sameElements(c.scope map (_.dom.size)),
        c + " was revised!")
    }

    true;
  }

  def key = keyType.getConstructor().newInstance()

  def queue = queueType.getConstructor(classOf[Key[Constraint]]).newInstance(key)
}

final class AC3Constraint(val problem: Problem, val queue: Queue[Constraint]) extends Filter with Loggable {
  @Statistic
  val substats = new StatisticsManager
  substats.register("ac.priorityQueue", queue);
  // private static final Logger LOGGER = Logger.getLogger(Filter.class
  // .getSimpleName());

  @Statistic
  var revisions = 0;

  def this(problem: Problem) = this(problem, AC3Constraint.queue)

  def reduceAll() = {
    Removals.clear()
    queue.clear();
    addAll();
    reduce();

  }

  def reduceFrom(modVar: Array[Int], modCons: Array[Int], cnt: Int) = {
    Removals.clear()
    queue.clear();
    // LOGGER.fine("reduce after " + cnt);
    for (v <- problem.variables) {
      if (modVar(v.getId) > cnt) {
        v.constraints.zipWithIndex.foreach {
          case (c, j) =>
            if (!c.isEntailed) {
              c match {
                case c: Removals =>
                  c.setRemovals(v.positionInConstraint(j));
              }

              queue.offer(c);
            }
        }
      }
    }

    if (modCons != null) {
      problem.constraints.foreach { c =>
        if (modCons(c.getId) > cnt && !c.isEntailed) {
          c.fillRemovals()
          queue.offer(c);
        }
      }
    }

    reduce();
  }

  def reduceAfter(variable: Variable) = {

    if (variable == null) {
      true;
    } else {
      Removals.clear()
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
      c.setRemovals(modified);
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

      if (constraint.consistentRevise()) {
        updateQueue(sizes, constraint, constraint.arity - 1)
        constraint.clearRemovals();
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
        c.fillRemovals()
        queue.offer(c);
      }
    }
  }

  override def toString = "AC-cons+" + queue.getClass().getSimpleName();

  def getStatistics = Map("revisions" -> revisions)

  def reduceAfter(constraints: Iterable[Constraint]) = {
    Removals.clear()
    queue.clear();

    for (c <- constraints if !c.isEntailed) {
      c.fillRemovals()
      queue.offer(c);
    }

    reduce();
  }

}
