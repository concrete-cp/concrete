package concrete.filter;

import scala.annotation.tailrec
import scala.reflect.runtime.universe
import com.typesafe.scalalogging.LazyLogging
import concrete.constraint.AdviseCount
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
import concrete.constraint.AdviseCounts
import concrete.constraint.Advisable
import concrete.constraint.Removals

object ACC extends LazyLogging {
  def control(problem: Problem) = {
    logger.debug("Control !")
    for (c <- problem.constraints) {
      //val before = c.toString
      (0 until c.arity).foreach(c.advise)
      //val sizes = c.scope map (_.dom.size)
      require(c.revise().isEmpty, s"$c was revised")
      //      assert(c.revise().isEmpty, s"$c was revised (was $before)")
      //      assert(
      //        sizes.sameElements(c.scope map (_.dom.size)),
      //        s"$c was revised and did not advertize it! (was $before)")
      require(c.controlAssignment, s"$c assignement is inconsistent")
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

  private val advises = new AdviseCount()

  problem.constraints.iterator.collect {
    case c: Advisable => c
  } foreach {
    _.register(advises)
  }

  @Statistic
  val substats = new StatisticsManager
  substats.register("queue", queue);
  // private static final Logger LOGGER = Logger.getLogger(Filter.class
  // .getSimpleName());

  @Statistic
  var revisions = 0;

  def reduceAll() = {
    advises.clear()
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
    advises.clear()
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

      logger.debug {
        s"$constraint " + (
          constraint match {
            case r: Removals => s"(${r.modified}) "
            case _ =>
          })
      }

//      val deb = s"$constraint " + (
//        constraint match {
//          case r: Removals => s"(${r.modified}) "
//          case _ =>
//        })

      val mod = try {
        constraint.revise()
      } catch {
        case _: UNSATException =>
          //println(s"$deb -> unsat")
          constraint.weight += 1
          return false
      }

      logger.debug(if (mod.isEmpty) "NOP" else s"-> $constraint")
      
      //if (mod.nonEmpty) println(s"$deb -> $constraint")

      mod.foreach(i => updateQueue(constraint.scope(i), constraint))

      reduce()
    }
  }

  override def toString = "AC-cons+" + queue.getClass().getSimpleName();

  def getStatistics = Map("revisions" -> revisions)

  def reduceAfter(constraints: Iterable[Constraint]) = {
    advises.clear()
    queue.clear();

    for (c <- constraints if !c.isEntailed)
      adviseAndEnqueueAll(c)

    reduce();
  }

}
