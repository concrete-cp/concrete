package concrete.filter;

import scala.Array.canBuildFrom
import scala.annotation.elidable
import scala.annotation.tailrec

import annotation.elidable.ASSERTION
import concrete.constraint.Constraint
import concrete.heuristic.revision.Dom
import concrete.heuristic.revision.Key
import concrete.priorityqueues.BinaryHeap
import concrete.priorityqueues.PriorityQueue
import com.typesafe.scalalogging.slf4j.LazyLogging
import concrete.Parameter
import cspom.Statistic
import concrete.AdviseCount
import concrete.ParameterManager
import concrete.Problem
import cspom.StatisticsManager
import concrete.UNSATException
import concrete.Variable

/**
 * @author scand1sk
 *
 */

final class ACV(
  val problem: Problem, params: ParameterManager) extends Filter with LazyLogging {

  @Parameter("ac3v.queue")
  var queueType: Class[_ <: PriorityQueue[Variable]] = classOf[BinaryHeap[Variable]]

  @Parameter("ac3v.key")
  var keyType: Class[_ <: Key[Variable]] = classOf[concrete.heuristic.revision.Dom]

  val key = keyType.getConstructor().newInstance()

  val queue = queueType.getConstructor().newInstance()

  @Statistic
  val substats = new StatisticsManager
  substats.register("queue", queue);

  // private static final Logger LOGGER = Logger.getLogger(Filter.class
  // .getSimpleName());

  @Statistic
  var revisions = 0;

  def reduceAll() = {
    queue.clear();
    problem.variables.foreach(v => queue.offer(v, key.getKey(v)))
    problem.constraints foreach AdviseCount.adviseAll
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
        a(i - 1)
      }

    }

    a(constraints.length - 1)

  }

  def updateQueue(mod: Traversable[Int], constraint: Constraint) {
    //assume(prev.length == constraint.arity)
    /** Requires high optimization */

    //val scope = constraint.scope

    for (p <- mod) {
      val variable = constraint.scope(p)
      queue.offer(variable, key.getKey(variable))
      advise(variable, constraint)
    }
  }

  @tailrec
  private def prepareQueue(modifiedConstraints: Iterator[Constraint]): Boolean = {
    if (modifiedConstraints.hasNext) {
      val c = modifiedConstraints.next
      AdviseCount.adviseAll(c)

      //val prev = c.sizes()

      val sat = try {
        updateQueue(c.revise(), c)
        //if (c.revise()) updateQueue(prev, c)
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

    def r(i: Int): Boolean = (i < 0) || {
      val c = constraints(i)

      (c.isEntailed || {
        revisions += 1
        //val prev = c.sizes()
        //println("Revising " + c)

        try {
          updateQueue(c.revise(), c)
          //          if (c.revise()) {
          //            assert(!(c.sizes() sameElements prev), c + " returned wrong true revised info")
          //            updateQueue(prev, c)
          //          } else assert(c.sizes() sameElements prev, c + " returned wrong false revised info")
          true
        } catch {
          case _: UNSATException =>
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
