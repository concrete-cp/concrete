package concrete.filter;

import scala.annotation.tailrec
import scala.reflect.runtime.universe
import com.typesafe.scalalogging.slf4j.LazyLogging
import concrete.AdviseCount
import concrete.ParameterManager
import concrete.Problem
import concrete.UNSATException
import concrete.Variable
import concrete.constraint.Constraint
import concrete.heuristic.revision.Eval
import concrete.priorityqueues.QuickFifos
import cspom.Statistic
import cspom.StatisticsManager
import concrete.priorityqueues.PriorityQueue
import concrete.heuristic.revision.Key

object ACC extends LazyLogging {
  def control(problem: Problem) = {
    logger.debug("Control !")
    for (c <- problem.constraints) {
      (0 until c.arity).foreach(c.advise)
      val sizes = c.scope map (_.dom.size)
      assert(c.revise().isEmpty, c + " was revised")
      assert(
        sizes.sameElements(c.scope map (_.dom.size)),
        c + " was revised!")
    }

    true;
  }

}

final class ACC(val problem: Problem, params: ParameterManager) extends Filter with LazyLogging {

  private val queueType: Class[_ <: PriorityQueue[Constraint]] =
    params.getOrElse("ac3c.queue", classOf[QuickFifos[Constraint]])

  private val keyType: Class[_ <: Key[Constraint]] =
    params.getOrElse("ac3c.key", classOf[Eval])

  private val key = keyType.getConstructor().newInstance()

  private val queue = queueType.getConstructor().newInstance()

  @Statistic
  val substats = new StatisticsManager
  substats.register("queue", queue);
  // private static final Logger LOGGER = Logger.getLogger(Filter.class
  // .getSimpleName());

  @Statistic
  var revisions = 0;

  def reduceAll() = {
    AdviseCount.clear()
    queue.clear()
    for (c <- problem.constraints if (!c.isEntailed))
      adviseAndEnqueueAll(c)

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
      for (c <- problem.constraints if (modCons(c.getId) > cnt && !c.isEntailed))
        adviseAndEnqueueAll(c)

    reduce();
  }

  private def adviseAndEnqueueAll(c: Constraint) {
    var p = c.arity - 1
    while (p >= 0) {
      val a = c.advise(p)
      if (a >= 0) queue.offer(c, key.getKey(c, a))
      p -= 1
    }
  }

  def reduceAfter(variable: Variable) = variable == null || {
    AdviseCount.clear()
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
          //logger.fine(c + ", " + modified.positionInConstraint(i) + " : " + a)
          if (a >= 0) queue.offer(c, key.getKey(c, a))
        }

        upd(i - 1)
      }
    }

    upd(constraints.length - 1)
  }

  @tailrec
  private def reduce(): Boolean = {
    if (queue.isEmpty) {
      assert(ACC.control(problem))
      true
    } else {
      val constraint = queue.poll();

      revisions += 1;
      //val sizes = constraint.sizes()

      val mod = try {
        constraint.revise()
      } catch {
        case _: UNSATException =>
          constraint.weight += 1
          return false
      }

      mod.foreach(i => updateQueue(constraint.scope(i), constraint))

      reduce()
    }
  }

  override def toString = "AC-cons+" + queue.getClass().getSimpleName();

  def getStatistics = Map("revisions" -> revisions)

  def reduceAfter(constraints: Iterable[Constraint]) = {
    AdviseCount.clear()
    queue.clear();

    for (c <- constraints if !c.isEntailed)
      adviseAndEnqueueAll(c)

    reduce();
  }

}
