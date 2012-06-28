package cspfj.filter;

import scala.annotation.tailrec

import cspfj.constraint.Constraint
import cspfj.constraint.Removals
import cspfj.priorityqueues._
import cspfj.util.Loggable
import cspfj.EmptyDomainException
import cspfj.ParameterManager
import cspfj.Problem
import cspfj.Statistic
import cspfj.StatisticsManager
import cspfj.UNSATException
import cspfj.Variable

object ACC extends Loggable {
  //  @Parameter("ac3c.queue")
  //  var queueType: Class[_ <: Queue[Constraint]] = classOf[QuickFifos]
  //
  //  @Parameter("ac3c.key")
  //  var keyType: Class[_ <: Key[Constraint]] = classOf[Eval]

  ParameterManager.register(ACC.this);

  def control(problem: Problem) = {
    logger.fine("Control !")
    for (c <- problem.constraints) {
      (0 until c.arity).foreach(c.advise)
      val sizes = c.scope map (_.dom.size)
      assert(!c.revise(), c + " was revised")
      assert(
        sizes.sameElements(c.scope map (_.dom.size)),
        c + " was revised!")
    }

    true;
  }

  //def key = keyType.getConstructor().newInstance()

  def queue = new QuickFifos //queueType.getConstructor(classOf[Key[Constraint]]).newInstance(key)
}

final class ACC(val problem: Problem, val queue: PriorityQueue[Constraint]) extends Filter with Loggable {
  @Statistic
  val substats = new StatisticsManager
  substats.register("ac.priorityQueue", queue);
  // private static final Logger LOGGER = Logger.getLogger(Filter.class
  // .getSimpleName());

  @Statistic
  var revisions = 0;

  def this(problem: Problem) = this(problem, ACC.queue)

  def reduceAll() = {
    Removals.clear()
    queue.clear()
    for (c <- problem.constraints if (!c.isEntailed)) {
      queue.offer(c, c.adviseAll());
    }
    reduce()
  }

  def reduceFrom(modVar: Array[Int], modCons: Array[Int], cnt: Int) = {
    //Removals.clear()
    queue.clear();

    for (
      v <- problem.variables if (modVar(v.getId) > cnt)
    ) {
      updateQueue(v, null)
    }

    if (modCons != null)
      for (c <- problem.constraints if (modCons(c.getId) > cnt && !c.isEntailed)) {
        val a = c.adviseAll()
        if (a >= 0) queue.offer(c, a);
      }

    reduce();
  }

  def reduceAfter(variable: Variable) = variable == null || {
    Removals.clear()
    queue.clear();
    updateQueue(variable, null)
    reduce();
  }

  private def updateQueue(modified: Variable, skip: Constraint) {
    val constraints = modified.constraints

    @tailrec
    def upd(i: Int) {
      if (i >= 0) {
        val c = constraints(i)

        if ((c ne skip) && !c.isEntailed) {
          val a = c.advise(modified.positionInConstraint(i))
          if (a >= 0) queue.offer(c, a)
        }

        upd(i - 1)
      }
    }

    upd(constraints.length - 1)
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
      assert(ACC.control(problem))
      true
    } else {
      val constraint = queue.poll();

      revisions += 1;
      val sizes = constraint.sizes()

      val sat = try {
        if (constraint.revise()) {
          assert(!(constraint.sizes() sameElements sizes), constraint + " returned wrong true revised info")
          updateQueue(sizes, constraint, constraint.arity - 1)
        } else assert(constraint.sizes() sameElements sizes, constraint + " returned wrong false revised info")
        true
      } catch {
        case e: UNSATException =>
          constraint.weight += 1
          false
      }

      sat && reduce()
    }
  }

  override def toString = "AC-cons+" + queue.getClass().getSimpleName();

  def getStatistics = Map("revisions" -> revisions)

  def reduceAfter(constraints: Iterable[Constraint]) = {
    Removals.clear()
    queue.clear();

    for (c <- constraints if !c.isEntailed) {
      val a = c.adviseAll()
      if (a >= 0) queue.offer(c, a);
    }

    reduce();
  }

}
