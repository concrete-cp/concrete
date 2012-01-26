package cspfj.filter;

import java.util.Queue
import scala.collection.IndexedSeq
import cspfj.ParameterManager
import cspfj.StatisticsManager
import cspfj.constraint.Constraint
import cspfj.problem.Problem
import cspfj.problem.Variable
import cspfj.Parameter
import cspfj.Statistic
import scala.annotation.tailrec
import cspfj.priorityqueues._
import cspfj.util.Loggable

object AC3Constraint {
  @Parameter("ac.queue")
  var queueType: Class[_ <: Queue[Constraint]] = classOf[ScalaIOBinomialHeap[Constraint]]

  @Parameter("ac.key")
  var key = new Key[Constraint]() {
    def getKey(o: Constraint) = o.getEvaluation

    override def toString = "object.getEvaluation"
  };

  @Statistic
  var revisionCount = 0

  ParameterManager.register(this);
}

final class AC3Constraint(val problem: Problem, val queue: Queue[Constraint]) extends Filter with Loggable {
  StatisticsManager.register("ac.priorityQueue", queue);
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
              c.setRemovals(v.positionInConstraint(j),
                AC3Constraint.revisionCount);

              queue.offer(c);
            }
        }
      }
    }

    if (modCons != null) {
      problem.constraints.foreach { c =>
        if (modCons(c.getId) > cnt && !c.isEntailed) {
          c.fillRemovals(AC3Constraint.revisionCount);

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
      variable.constraints.iterator.zipWithIndex.foreach {
        case (c, cp) =>
          if (!c.isEntailed) {
            c.setRemovals(
              variable.positionInConstraint(cp), AC3Constraint.revisionCount);
            queue.offer(c);
          }
      }
      //info("reduce " + AC3Constraint.revisionCount)
      reduce();
    }
  }

  private def updateQueue(sizes: Seq[Int], constraint: Constraint) {
    (constraint.scope, sizes).zipped.foreach { (v, s) =>
      if (v.dom.size != s)
        v.constraints.iterator.zipWithIndex.foreach {
          case (c, cp) =>
            if (c != constraint && !c.isEntailed) {
              c.setRemovals(v.positionInConstraint(cp), AC3Constraint.revisionCount);
              queue.offer(c);
            }

        }
    }
  }

  @tailrec
  private def reduce(): Boolean = {
    if (queue.isEmpty) {
      assert(control)
      true
    } else {
      val constraint = queue.poll();

      revisions += 1;
      val sizes = constraint.sizes
      if (constraint.revise(AC3Constraint.revisionCount)) {
        updateQueue(sizes, constraint)
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

  private def control() = {

    for (c <- problem.constraints) {
      val sizes = c.scope map (_.dom.size)
      assert(c.revise(-1));
      assert(sizes.sameElements(c.scope map (_.dom.size)))
    }
    true;
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
