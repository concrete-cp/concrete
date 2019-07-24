package concrete.filter

import com.typesafe.scalalogging.LazyLogging
import concrete._
import concrete.constraint.{AdviseCount, Constraint, StatefulConstraint}
import concrete.heuristic.ContradictionEvent
import concrete.heuristic.revision.{Eval, Key}
import concrete.priorityqueues.{PriorityQueue, QuickFifos}
import cspom.{Statistic, StatisticsManager}

object ACC extends LazyLogging {
  def control(problem: Problem, state: ProblemState): Option[Constraint] = {
    logger.info("Control !")

    problem.constraints.find(c => !c.controlRevision(state))
    // problem.constraints.find(c => c.cardSize(state) > 1 && !c.controlRevisionDeep(state))
  }

}

final class ACC(val problem: Problem, params: ParameterManager) extends Filter with LazyLogging {

  type Cause = Set[Variable]
  @Statistic
  val substats = new StatisticsManager
  private val queueType: Class[_ <: PriorityQueue[Constraint]] =
    params.classInPackage("ac3c.queue", "concrete.priorityqueues", classOf[QuickFifos[Constraint]])
  private val keyType: Class[_ <: Key[Constraint]] =
    params.classInPackage("ac3c.key", "concrete.heuristic.revision", classOf[Eval])
  private val key = keyType.getConstructor().newInstance()
  private val queue = queueType.getConstructor().newInstance()

  //  var active = Set[Int]()
  private val advises = new AdviseCount()

  problem.constraints.foreach(addConstraint)
  @Statistic
  var revisions = 0L
  substats.register("queue", queue)

  def addConstraint[A <: Constraint](c: A): A = {
    c.register(advises)
  }

  def reduceAll(states: ProblemState): Outcome = {
    advises.clear()
    queue.clear()

    for (c <- problem.constraints) {
      if (!states.entailed.hasInactiveVar(c)) {
        adviseAndEnqueue(c, states)
      }
    }

    reduce(states)
  }

  def reduceFrom(modVar: Array[Int], modCons: Array[Int], cnt: Int, states: ProblemState): Outcome = {
    queue.clear()

    for (
      v <- problem.variables
    ) {
      if (modVar(v.id) > cnt) updateQueue(v, BoundRemoval, null, states)
    }

    if (modCons != null) {
      for (i <- problem.constraints.indices) {
        val c = problem.constraints(i)
        if (modCons(i) > cnt && !states.entailed.hasInactiveVar(c)) {
          adviseAndEnqueue(c, states)
        }
      }
    }

    reduce(states)
  }

  def reduceAfter(modifV: Seq[(Variable, Event)], modifC: Iterable[Constraint], states: ProblemState): Outcome = {
    advises.clear()
    queue.clear()
    for ((v, e) <- modifV) {
      updateQueue(v, e, null, states)
    }
    for (c <- modifC) {
      //if (!states.entailed.hasInactiveVar(c)) {
      adviseAndEnqueue(c, states)
      //}
    }
    reduce(states)
  }

  override def toString: String = "AC-cons+" + queue.getClass.getSimpleName

  private def adviseAndEnqueue(c: Constraint, state: ProblemState): Unit = {
    val max = c.eventAll(state, Assignment)

    if (max >= 0) {
      //println(max + " : " + c)
      queue.offer(c, key.getKey(c, state, max))
    }
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
        val a = c.eventArray(states, event, positions)
        enqueue(c, a, states)
      } else if (c ne skip) {
        val a = c.event(states, event, positions(0))
        enqueue(c, a, states)
      }

      //logger.fine(c + ", " + modified.positionInConstraint(i) + " : " + a)

      i = active.nextSetBit(i + 1)
    }

  }

  private def enqueue(c: Constraint, a: Int, states: ProblemState): Unit = {
    //logger.trace(s"Queueing ${c.id}. ${c.toString(states)}, advise = $a")
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

      revisions += 1

      constraint.revise(s.clearRecent) match {
        case c: Contradiction =>

          logger.debug(s"${constraint.id}.${constraint.weight}. ${constraint.toString(s)} -> Contradiction")

          val nc = if (c.from.isEmpty) {
            c dueTo ((constraint, constraint.scope))
          } else {
            c
          }

          contradictionListeners.foldLeft(nc: Outcome) { case (p, l) => l.event(ContradictionEvent, p) }

        case newState: ProblemState =>
          // logger.debug(s"$s -> $newState")
          if (newState.recentUpdates.nonEmpty) {
            logger.debug(
              s"${constraint.id}.${constraint.weight}. ${constraint.toString(s)} -> ${
                constraint.diff(s, newState).map { case (v, (_, a)) => s"$v <- $a" }.mkString(", ")
              }${
                if (newState.entailed.hasInactiveVar(constraint)) " - entailed" else ""
              }")

            assert(constraint.scope.forall(v => newState.dom(v).nonEmpty))

            assert(constraint.controlAssignment(newState), s"${constraint.toString(newState)} assignement is inconsistent")

            // Do not control revision now, as multiple instances of a variable in a scope max require
            // to reach a fixpoint beforehand
            //assert(constraint.controlRevision(newState), s"Revision control failed for ${constraint.toString(s)}")

            for ((i, after) <- newState.recentUpdates) {
              val before = s.dom(i)
              assert(after.size < before.size)
              val e = InsideRemoval(before, after)
              updateQueue(problem.variables(i), e, constraint, newState)
            }

          } else if (newState ne s) {

            logger.debug(s"${constraint.id}.${constraint.weight}. ${constraint.toString(s)} -> ${
              constraint match {
                case sc: StatefulConstraint[_] => s"new state: ${newState(sc)}"
                case _ => "NOP"
              }
            }${if (newState.entailed.hasInactiveVar(constraint)) " - entailed" else ""}")

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

}
