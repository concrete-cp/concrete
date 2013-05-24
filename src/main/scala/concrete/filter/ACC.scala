package concrete.filter;

import scala.annotation.tailrec
import concrete.constraint.Constraint
import concrete.constraint.Removals
import concrete.priorityqueues._
import concrete.util.Loggable
import concrete.EmptyDomainException
import concrete.ParameterManager
import concrete.Problem
import concrete.Statistic
import concrete.StatisticsManager
import concrete.UNSATException
import concrete.Variable
import concrete.AdviseCount
import concrete.Parameter
import concrete.heuristic.revision.Key
import concrete.heuristic.revision.Eval

object ACC extends Loggable {
  @Parameter("ac3c.queue")
  var queueType: Class[_ <: PriorityQueue[Constraint]] = classOf[QuickFifos[Constraint]]

  @Parameter("ac3c.key")
  var keyType: Class[_ <: Key[Constraint]] = classOf[Eval]

  ParameterManager.register(ACC.this);

  def control(problem: Problem) = {
    logger.fine("Control !")
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

  def key = keyType.getConstructor().newInstance()

  def queue = queueType.getConstructor().newInstance()
}

final class ACC(val problem: Problem, val key: Key[Constraint], val queue: PriorityQueue[Constraint]) extends Filter with Loggable {
  @Statistic
  val substats = new StatisticsManager
  substats.register("queue", queue);
  // private static final Logger LOGGER = Logger.getLogger(Filter.class
  // .getSimpleName());

  @Statistic
  var revisions = 0;

  def this(problem: Problem) = this(problem, ACC.key, ACC.queue)

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

  private def updateQueue(mod: Traversable[Int], constraint: Constraint) {
    mod.foreach(i => updateQueue(constraint.scope(i), constraint))
    //    if (i >= 0) {
    //      val v = constraint.scope(i)
    //
    //      if (v.dom.size != sizes(i))
    //        updateQueue(v, constraint)
    //
    //      updateQueue(sizes, constraint, i - 1)
    //    }
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

      val sat = try {
        updateQueue(constraint.revise(), constraint)
        //        if (constraint.revise()) {
        //          assert(!(constraint.sizes() sameElements sizes), constraint + " returned wrong true revised info")
        //          updateQueue(sizes, constraint, constraint.arity - 1)
        //        } else assert(constraint.sizes() sameElements sizes, constraint + " returned wrong false revised info")
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
    AdviseCount.clear()
    queue.clear();

    for (c <- constraints if !c.isEntailed)
      adviseAndEnqueueAll(c)

    reduce();
  }

}
