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
import concrete.constraint.StatefulConstraint
import concrete.heuristic.revision.Eval
import concrete.priorityqueues.QuickFifos
import cspom.Statistic
import cspom.StatisticsManager
import concrete.priorityqueues.PriorityQueue
import concrete.heuristic.revision.Key
import concrete.Event
import concrete.BoundRemoval
import concrete.InsideRemoval

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

  //  var active = Set[Int]()

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

    //println("reduce all")

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
      if (modVar(v.id) > cnt) updateQueue(v, BoundRemoval, null, states)
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

  def reduceAfter(modif: Seq[(Variable, Event)], states: ProblemState) = {
    advises.clear()
    queue.clear();
    for ((v, e) <- modif) {
      updateQueue(v, e, null, states)
    }
    reduce(states);
  }

  private def updateQueue(modified: Variable, event: Event, skip: Constraint, states: ProblemState): Unit = {

    // logger.debug(s"Modified $modified, queueing ${modified.constraints.map(_.toString(states)).mkString("{", ", ", "}")}, skipping ${skip.toString(states)}")

    val constraints = modified.constraints
    val active = states.activeConstraints(modified)

    var i = active.nextSetBit(0)
    while (i >= 0) {
      val c = constraints(i)
      val positions = modified.positionInConstraint(i)

      logger.trace(s"$modified at [${positions.mkString(", ")}]: enqueuing ${c.id}. ${c.toString(states)}")

      if (positions.length > 1) {
        //println(s"$modified at ${positions.toSeq}: enqueuing ${c.toString(states)}")
        val a = c.adviseArray(states, event, positions)
        enqueue(c, a, states)
      } else if (c ne skip) {
        val a = c.advise(states, event, positions(0))
        enqueue(c, a, states)
      }

      //logger.fine(c + ", " + modified.positionInConstraint(i) + " : " + a)

      i = active.nextSetBit(i + 1)
    }

  }

  private def enqueue(c: Constraint, a: Int, states: ProblemState): Unit = {
    logger.trace(s"Queueing ${c.id}. ${c.toString(states)}, advise = $a")
    if (a >= 0) queue.offer(c, key.getKey(c, states, a))
  }

  @annotation.tailrec
  private def reduce(s: ProblemState): Outcome = {
    if (queue.isEmpty) {
      assert {
        val errors = ACC.control(problem, s)
        errors.foreach(c => logger.error(s"ACC control failed on ${c.id}. ${c.toString(s)}"))
        errors.isEmpty
      }
      s
    } else {
      val constraint = queue.poll()

      revisions += 1;

      constraint.revise(s) match {
        case c: Contradiction =>
          logger.debug(s"${constraint.id}.${constraint.weight}. ${constraint.toString(s)} -> Contradiction")

          val nc = if (c.from.isEmpty) {
            c dueTo ((constraint, constraint.scope))
          } else {
            c
          }

          for (l <- contradictionListener) l(nc)
          nc

        case newState: ProblemState =>
          if (newState.domains ne s.domains) {
            logger.debug(
              s"${constraint.id}.${constraint.weight}. ${constraint.toString(s)} -> ${constraint.toString(newState)}${if (newState.isEntailed(constraint)) " - entailed" else ""}")

            assert(constraint.scope.forall(v => newState.dom(v).nonEmpty))

            assert(constraint.controlAssignment(newState), s"${constraint.toString(newState)} assignement is inconsistent")

            // Do not control revision now, as multiple instances of a variable in a scope max require
            // to reach a fixpoint beforehand
            //assert(constraint.controlRevision(newState), s"Revision control failed for ${constraint.toString(s)}")

            var p = constraint.arity - 1
            val scope = constraint.scope
            while (p >= 0) {
              val v = scope(p)

              val before = s.dom(v)
              val after = newState.dom(v)

              if (before ne after) {
                val e = InsideRemoval(before, after)
                updateQueue(v, e, constraint, newState)
              }

              p -= 1
            }

          } else if (newState ne s) {

            logger.debug(s"${constraint.id}.${constraint.weight}. ${constraint.toString(s)} -> ${
              constraint match {
                case sc: StatefulConstraint[_] => s"new state: ${newState(sc)}"
                case _ => "NOP"
              }
            }${if (newState.isEntailed(constraint)) " - entailed" else ""}")

            //assert(constraint.controlRevision(newState))

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
