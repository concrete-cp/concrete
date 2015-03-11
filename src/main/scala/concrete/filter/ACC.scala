package concrete.filter;

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
import concrete.constraint.Removals
import concrete.heuristic.revision.Eval
import concrete.priorityqueues.QuickFifos
import cspom.Statistic
import cspom.StatisticsManager
import concrete.priorityqueues.PriorityQueue
import concrete.heuristic.revision.Key

object ACC extends LazyLogging {
  def control(problem: Problem, state: ProblemState): ProblemState = {
    logger.info("Control !")
    problem.constraints.foldLeft(state) {
      (s, c) =>

        //println(s"Controlling $c")

        c.adviseAll(s)

        c.revise(s) match {
          case Contradiction => throw new AssertionError(s"${c.toString(s)} is not consistent${if (s.isEntailed(c)) " - entailed" else ""}")
          case finalState: ProblemState =>
            require(c.scope.forall(v => s.dom(v) eq finalState.dom(v)),
              s"${c.toString(state)}${if (state.isEntailed(c)) " - entailed" else ""} was revised (-> ${c.toString(finalState)})")

            require(c.controlAssignment(finalState), s"${c.toString(finalState)} assignement is inconsistent")
            finalState
        }

    }

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

  def reduceAll(states: ProblemState): Outcome = {
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
        if (modCons(i) > cnt && !states.entailed(i)) {
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

  def reduceAfter(variable: Variable, states: ProblemState) = {
    if (variable == null) {
      states
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

      if ((c ne skip) && !states.isEntailed(c)) {
        val a = c.advise(states, modified.positionInConstraint(i))

        //logger.fine(c + ", " + modified.positionInConstraint(i) + " : " + a)
        if (a >= 0) queue.offer(c, key.getKey(c, states, a))
      }

      i -= 1

    }

  }

  private def reduce(states: ProblemState): Outcome = {
    lazy val varSet = problem.variables.toSet
    var s = states
    while (!queue.isEmpty) {
      val constraint = queue.poll();
      //println(constraint)

      revisions += 1;
      //val sizes = constraint.sizes()

      constraint.revise(s) match {
        case Contradiction =>
          constraint.weight += 1
          logger.debug(constraint.toString(s) + " -> Contradiction")
          return Contradiction

        case newState: ProblemState =>
          assert(constraint.scope.forall(v => newState.dom(v).nonEmpty))

          assert(noChange(s, newState, varSet -- constraint.scope), s"$constraint changed a variable outside of its scope")
          //          for (i <- 0 until constraint.arity) {
          //            if (newState(constraint.scope(i)).isEmpty) {
          //              constraint.weight += 1
          //              logger.info(
          //                s"${constraint.toString(s)} -> empty domain with state $newState")
          //              return Contradiction
          //            }
          //          }

          if (newState ne s) {
            logger.debug(
              s"${constraint.toString(s)} -> ${constraint.toString(newState)}${if (newState.isEntailed(constraint)) " - entailed" else ""}")

            for (i <- 0 until constraint.arity) {
              val v = constraint.scope(i)
              val id = v.id
              /* id will be < 0 if a fake variable is used by the constraint */
              if (id >= 0 && (newState.dom(id) ne s.dom(id))) {
                assert(newState.dom(id).subsetOf(s.dom(id)))

                updateQueue(v, constraint, newState)
              }
            }
            s = newState
          } else {
            logger.debug(
              s"${constraint.toString(s)} -> NOP")
          }

      }

      //println(mod)

    }

    assert {
      s = ACC.control(problem, s)
      true
    }
    s

  }

  private def noChange(oldState: ProblemState, newState: ProblemState, variables: Set[Variable]): Boolean = {
    variables.forall(v => oldState.dom(v) eq newState.dom(v))
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
