package concrete.filter;

import scala.annotation.tailrec
import scala.reflect.runtime.universe
import com.typesafe.scalalogging.LazyLogging
import concrete.ParameterManager
import concrete.Problem
import concrete.UNSATException
import concrete.Variable
import concrete.constraint.Advisable
import concrete.constraint.AdviseCount
import concrete.constraint.Constraint
import concrete.constraint.Removals
import concrete.heuristic.revision.Eval
import cspom.Statistic
import cspom.StatisticsManager
import concrete.priorityqueues.PriorityQueue
import concrete.priorityqueues.QuickFifos
import concrete.heuristic.revision.Key
import concrete.Contradiction
import concrete.Filtered
import concrete.FilterOutcome
import concrete.Revised
import concrete.FilterOutcome
import concrete.ProblemState

object ACC extends LazyLogging {
  def control(problem: Problem, state: ProblemState): ProblemState = {
    logger.info("Control !")
    var s = state
    for (c <- problem.constraints) {

      val id = c.id

      val oldState: c.State = s.constraintState(id).asInstanceOf[c.State]
      val oldDomains = state.domains(c.scope)
      c.adviseAll(oldDomains)

      val sizes = oldDomains map (_.size)

      c.revise(oldDomains, oldState) match {
        case Contradiction => throw new AssertionError(s"${c.toString(oldDomains, oldState)} is not consistent")
        case Revised(modified, entailed, finalState) =>
          require((modified, oldDomains).zipped.forall(_ eq _),
            s"${c.toString(oldDomains, oldState)} was revised (-> $modified, $finalState)")
          s = s.updatedCS(id, finalState)
      }

      require(c.controlAssignment(oldDomains), s"$c assignement is inconsistent")

    }

    s
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

  def reduceAll(states: ProblemState): FilterOutcome = {
    advises.clear()
    queue.clear()

    for (c <- problem.constraints)
      if (!states.isEntailed(c)) {
        adviseAndEnqueue(c, states)
      }
    //    
    //    val advisedStates: ProblemState = states.updateConstraints {
    //      (i, s) =>
    //        val c = problem.constraints(i)
    //        if (c.isEntailed) {
    //          s
    //        } else {
    //          adviseAndEnqueue(c)(s.asInstanceOf[c.State])
    //        }
    //    }

    reduce(states)
  }

  def reduceFrom(modVar: Array[Int], modCons: Array[Int], cnt: Int, states: ProblemState): FilterOutcome = {
    //Removals.clear()
    queue.clear();

    for (
      v <- problem.variables
    ) {
      if (modVar(v.id) > cnt) updateQueue(v, null, states)
    }

    if (modCons != null) {
      for (i <- 0 until problem.constraints.length) {
        val c = problem.constraints(i)
        if (modCons(i) > cnt && !states.entailed(i)) {
          adviseAndEnqueue(c, states)
        }
      }
    }

    reduce(states);
  }

  private def adviseAndEnqueue(c: Constraint, state: ProblemState): Unit = {
    val max = c.adviseAll(state.domains(c.scope))
    if (max >= 0) {
      //println(max + " : " + c)
      queue.offer(c, key.getKey(c, state, max))
    }
  }

  def reduceAfter(variable: Variable, states: ProblemState) = {
    if (variable == null) {
      Filtered(states)
    } else {
      advises.clear()
      queue.clear();
      updateQueue(variable, null, states)
      reduce(states);
    }
  }

  private def updateQueue(modified: Variable, skip: Constraint, states: ProblemState): Unit = {
    val constraints = modified.constraints

    var i = constraints.length - 1
    while (i >= 0) {
      val c = constraints(i)

      if ((c ne skip) && !states.entailed(i)) {
        val a = c.advise(states.domains(c.scope), modified.positionInConstraint(i))

        //logger.fine(c + ", " + modified.positionInConstraint(i) + " : " + a)
        if (a >= 0) queue.offer(c, key.getKey(c, states, a))
      }

      i -= 1

    }

  }

  private def reduce(states: ProblemState): FilterOutcome = {

    var s = states
    while (!queue.isEmpty) {
      val constraint = queue.poll();
      //println(constraint)

      revisions += 1;
      //val sizes = constraint.sizes()

      logger.debug {
        s"$constraint " + (
          constraint match {
            case r: Removals => s"(${r.modified}) "
            case _           =>
          })
      }

      val constraintDomains = s.domains(constraint.scope)
      val constraintState = s(constraint)

      constraint.revise(constraintDomains, constraintState) match {
        case Contradiction =>
          constraint.weight += 1
          logger.info(constraint.toString(constraintDomains, constraintState) + " -> Contradiction")
          return Contradiction

        case Revised(mod, entail, newState) =>
          for (i <- 0 until constraint.arity) {
            if (mod(i).isEmpty) {
              constraint.weight += 1
              logger.info(
                s"${constraint.toString(constraintDomains, constraintState)} -> empty domain with state $newState")
              return Contradiction
            }
          }

          val ns = s.updatedDomains(constraint.scope, mod)
          if (ns ne s) {
            logger.info(
              s"${constraint.toString(constraintDomains, constraintState)} -> ${constraint.toString(mod, newState)} ($entail)")
            s = ns
            for (i <- 0 until constraint.arity) {
              assert(mod(i).subsetOf(constraintDomains(i)),
                s"${constraint.toString(constraintDomains, constraintState)} -> ${constraint.toString(mod, newState)}")
              if (constraintDomains(i) ne mod(i)) {
                updateQueue(constraint.scope(i), constraint, s)
              }
            }
          } else {
            logger.info(
              s"${constraint.toString(constraintDomains, constraintState)} -> NOP with state $newState ($entail)")
          }
          if (newState != constraintState) {
            s = s.updatedCS(constraint, newState)
          }

      }

      //println(mod)

    }

    assert {
      s = ACC.control(problem, s)
      true
    }
    Filtered(s)

  }

  def domSizes(c: Constraint, state: ProblemState) = state.domains(c.scope).map(_.size)

  override def toString = "AC-cons+" + queue.getClass().getSimpleName();

  def getStatistics = Map("revisions" -> revisions)

  def reduceAfter(constraints: Iterable[Constraint], states: ProblemState) = {
    advises.clear()
    queue.clear();

    for (c <- constraints) {
      if (!states.isEntailed(c)) {
        adviseAndEnqueue(c, states)
      }
    }

    reduce(states);
  }

}
