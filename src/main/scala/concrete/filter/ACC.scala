package concrete.filter;

import scala.reflect.runtime.universe
import com.typesafe.scalalogging.LazyLogging
import concrete.Contradiction
import concrete.Outcome
import concrete.ParameterManager
import concrete.Problem
import concrete.ProblemState
import concrete.Variable
import concrete.constraint.Advisable
import concrete.constraint.AdviseCount
import concrete.constraint.Constraint
import concrete.constraint.StatefulConstraint
import concrete.heuristic.revision.Eval
import concrete.priorityqueues.QuickFifos
import cspom.Statistic
import cspom.StatisticsManager
import concrete.priorityqueues.PriorityQueue
import concrete.heuristic.revision.Key

object ACC extends LazyLogging {
  def control(problem: Problem, state: ProblemState): Option[Constraint] = {
    logger.info("Control !")

    problem.constraints.find(c => !c.controlRevision(state))
  }

}

final class ACC(val problem: Problem, params: ParameterManager) extends Filter with LazyLogging {

  private val queueType: Class[_ <: PriorityQueue[Constraint]] =
    params.classInPackage("ac3c.queue", "concrete.priorityqueues", classOf[QuickFifos[Constraint]])

  private val keyType: Class[_ <: Key[Constraint]] =
    params.classInPackage("ac3c.key", "concrete.heuristic.revision", classOf[Eval])

  private val key = keyType.getConstructor().newInstance()

  private val queue = queueType.getConstructor().newInstance()

  private val advises = new AdviseCount()

  problem.constraints.iterator
    .collect {
      case c: Advisable => c
    }
    .foreach(_.register(advises))

  @Statistic
  val substats = new StatisticsManager
  substats.register("queue", queue);

  @Statistic
  var revisions = 0L;

  def reduceAll(states: ProblemState): Outcome = {
    advises.clear()
    queue.clear()

    for (c <- problem.constraints) {
      if (!states.isEntailed(c)) {
        adviseAndEnqueue(c, states)
      }
    }

    reduce(states)
  }

  def reduceFrom(modVar: Array[Int], modCons: Array[Int], cnt: Int, states: ProblemState): Outcome = {
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
        if (modCons(i) > cnt && !states.isEntailed(c)) {
          adviseAndEnqueue(c, states)
        }
      }
    }

    reduce(states);
  }

  private def adviseAndEnqueue(c: Constraint, state: ProblemState): Unit = {
    val max = c.adviseAll(state)

    if (max >= 0) {
      //println(max + " : " + c)
      queue.offer(c, key.getKey(c, state, max))
    }
  }

  def reduceAfter(variables: Seq[Variable], states: ProblemState) = {
    advises.clear()
    queue.clear();
    for (v <- variables) {
      updateQueue(v, null, states)
    }
    reduce(states);
  }

  private def updateQueue(modified: Variable, skip: Constraint, states: ProblemState): Unit = {

    // logger.debug(s"Modified $modified, queueing ${modified.constraints.map(_.toString(states)).mkString("{", ", ", "}")}, skipping ${skip.toString(states)}")

    val constraints = modified.constraints

    for (i <- states.activeConstraints(modified)) {
      val c = constraints(i)

      val positions = modified.positionInConstraint(i)

      val a = if (positions.length > 1) {
        c.adviseArray(states, positions)
      } else if (c ne skip) {
        c.advise(states, positions(0))
      } else {
        -1
      }

      logger.trace(s"Queueing ${c.id}. ${c.toString(states)}, positions = ${positions.mkString("[", ", ", "]")}, advise = $a")
      //logger.fine(c + ", " + modified.positionInConstraint(i) + " : " + a)
      if (a >= 0) queue.offer(c, key.getKey(c, states, a))

    }

  }

  @annotation.tailrec
  private def reduce(s: ProblemState): Outcome = {
    if (queue.isEmpty) {
      assert {
        val errors = ACC.control(problem, s)
        errors.foreach(c => logger.error(s"ACC control failed on ${c.toString(s)}"))
        errors.isEmpty
      }
      s
    } else {
      val constraint = queue.poll()

      revisions += 1;

      constraint.revise(s) match {
        case Contradiction =>
          logger.debug(s"${constraint.id}.${constraint.weight}. ${constraint.toString(s)} -> Contradiction")
          constraint.weight += 1
          Contradiction

        case newState: ProblemState =>
          if (newState.domains ne s.domains) {
            logger.debug(
              s"${constraint.id}.${constraint.weight}. ${constraint.toString(s)} -> ${constraint.toString(newState)}${if (newState.isEntailed(constraint)) " - entailed" else ""}")

            assert(constraint.scope.forall(v => newState.dom(v).nonEmpty))

            assert(constraint.controlAssignment(newState), s"${constraint.toString(newState)} assignement is inconsistent")

            assert(constraint.controlRevision(newState), s"Revision control failed for ${constraint.toString(s)}")

            var p = constraint.arity - 1
            val scope = constraint.scope
            while (p >= 0) {
              val v = scope(p)

              if (newState.dom(v) ne s.dom(v)) {
                assert(newState.dom(v).subsetOf(s.dom(v)))

                updateQueue(v, constraint, newState)
              }
              p -= 1
            }

          } else if (newState ne s) {

            logger.debug(s"${constraint.id}.${constraint.weight}. ${constraint.toString(s)} -> ${
              constraint match {
                case sc: StatefulConstraint[_] => s"new state: ${newState(sc)}"
                case _                         => "NOP"
              }
            }${if (newState.isEntailed(constraint)) " - entailed" else ""}")

            assert(constraint.controlRevision(newState))

          } else {
            logger.debug(
              s"${constraint.id}.${constraint.weight}. ${constraint.toString(s)} -> NOP")

          }
          reduce(newState)

      }

      //println(mod)

    }

  }

  override def toString = "AC-cons+" + queue.getClass().getSimpleName();

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
