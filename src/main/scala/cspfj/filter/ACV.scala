package cspfj.filter;

import java.util.Queue
import scala.annotation.tailrec
import cspfj.constraint.Constraint
import cspfj.constraint.Removals
import cspfj.priorityqueues.Fifos
import cspfj.priorityqueues.ScalaNative
import cspfj.Problem
import cspfj.Variable
import cspfj.util.Loggable
import cspfj.Parameter
import cspfj.ParameterManager
import cspfj.Statistic
import cspfj.priorityqueues.BinomialHeap
import cspfj.priorityqueues.BinaryHeap
import cspfj.UNSATException
import cspfj.priorityqueues.PriorityQueue
import cspfj.heuristic.revision.Key
import cspfj.AdviseCount

/**
 * @author scand1sk
 *
 */
object ACV {
  @Parameter("ac3v.queue")
  var queueType: Class[_ <: PriorityQueue[Variable]] = classOf[BinaryHeap[Variable]]

  @Parameter("ac3v.key")
  val keyType: Class[_ <: Key[Variable]] = classOf[cspfj.heuristic.revision.Dom]

  def key = keyType.getConstructor().newInstance()

  def queue = queueType.getConstructor().newInstance()

  ParameterManager.register(ACV.this);
}

final class ACV(
  val problem: Problem,
  val queue: PriorityQueue[Variable]) extends Filter with Loggable {

  // private static final Logger LOGGER = Logger.getLogger(Filter.class
  // .getSimpleName());
  val key = ACV.key

  @Statistic
  var revisions = 0;

  def this(problem: Problem) =
    this(problem, ACV.queue)

  def reduceAll() = {
    queue.clear();
    problem.variables.foreach(v => queue.offer(v, key.getKey(v)))
    problem.constraints.foreach { c =>
      (0 until c.arity).foreach(c.advise)
    }

    reduce()

  }

  private def prepareQueue(v: Variable) {
    queue.offer(v, key.getKey(v))
    v.constraints.zipWithIndex.foreach {
      case (c, i) =>
        c.advise(v.positionInConstraint(i))
    }
  }

  private def advise(v: Variable, skip: Constraint) {

    val constraints = v.constraints

    @tailrec
    def a(i: Int) {
      if (i >= 0) {
        val c = constraints(i)
        if (c ne skip) c.advise(v.positionInConstraint(i))
      }
      a(i - 1)
    }

    a(constraints.length - 1)

  }

  def updateQueue(prev: Array[Int], constraint: Constraint) {
    /** Requires high optimization */

    val scope = constraint.scope

    @tailrec
    def p(i: Int) {
      if (i >= 0) {
        val variable = scope(i)
        if (prev(i) != variable.dom.size) {
          queue.offer(variable, key.getKey(variable))
          advise(variable, constraint)
        }
      }
      p(i - 1)
    }
    p(prev.length)

  }

  @tailrec
  private def prepareQueue(modifiedConstraints: Iterator[Constraint]): Boolean = {
    if (modifiedConstraints.hasNext) {
      val c = modifiedConstraints.next
      c.adviseAll()

      val prev = c.sizes()

      val sat = try {
        if (c.revise()) updateQueue(prev, c)
        true
      } catch {
        case _: UNSATException => {
          c.weight += 1
          false
        }
      }

      sat && prepareQueue(modifiedConstraints)

    } else true

  }

  def reduceAfter(constraints: Iterable[Constraint]): Boolean = {
    AdviseCount.clear()
    queue.clear()
    prepareQueue(constraints.iterator) && reduce()
  }

  def reduceAfter(variable: Variable) = variable == null || {
    AdviseCount.clear()
    queue.clear()
    prepareQueue(variable)
    reduce()
  }

  def reduceFrom(modVar: Array[Int], modCons: Array[Int], cnt: Int): Boolean = {
    AdviseCount.clear()
    queue.clear();
    // LOGGER.fine("reduce after " + cnt);

    for (v <- problem.variables if modVar(v.getId) > cnt)
      prepareQueue(v)

    if (modCons == null ||
      prepareQueue(problem.constraints.iterator.filter(c => modCons(c.getId) > cnt))) {
      reduce()
    } else {
      false
    }

  }

  @tailrec
  private def reduce(): Boolean = {
    // LOGGER.finer("Reducing");
    if (queue.isEmpty) {
      assert(ACC.control(problem))
      true
    } else {
      val variable = queue.poll()
      //info(variable.toString)
      reduce(variable.constraints) && reduce()
    }

  }

  private def reduce(constraints: Array[Constraint]) = {

    def r(i: Int): Boolean = {
      val c = constraints(i)

      (c.isEntailed || {
        revisions += 1
        val prev = c.sizes()
        //println("Revising " + c)

        try {
          if (c.revise()) {
            assert(!(c.sizes() sameElements prev), c + " returned wrong true revised info")
            updateQueue(prev, c)
          } else assert(c.sizes() sameElements prev, c + " returned wrong false revised info")
          true
        } catch {
          case e: UNSATException =>
            c.weight += 1
            false
        }
      }) && r(i - 1)
    }
    r(constraints.length - 1)
  }
  override def toString =
    "GAC3rm-var-" + queue.getClass.getSimpleName

  def getStatistics = Map("Revisions" -> revisions)

}
