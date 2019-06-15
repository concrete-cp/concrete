package concrete.constraint.semantic.energy

import bitvectors.BitVector
import concrete._
import concrete.constraint.Constraint
import concrete.constraint.semantic.CumulativeChecker


case class Instance(tasks: Array[Task], capa: Int)

class CumulativeEnergy2(
                         startingTimes: Array[Variable],
                         heights: Array[Int],
                         processingTimes: Array[Int],
                         capacity: Int)
  extends Constraint(startingTimes)
    with CumulativeChecker {


  private var filteringAlgorithm: BinarySearchPropagator = _

  private var positiveInstance: Instance = _
  private var negativeInstance: Instance = _
  private var tasks: Array[Task] = _
  private var negativeTasks: Array[Task] = _

  override def init(ps: ProblemState): Outcome = {
    tasks = for ((sT, i) <- startingTimes.zipWithIndex) yield {
      val est = ps.dom(sT).head
      val lct = ps.dom(sT).last + processingTimes(i)
      new Task(i + 1, est, lct, processingTimes(i), heights(i))
    }

    val maxLct = tasks.map(_.lct).max

    negativeTasks = for ((sT, i) <- startingTimes.zipWithIndex) yield {
      val est = ps.dom(sT).head
      val lct = ps.dom(sT).last + processingTimes(i)
      new Task(i + 1, -lct + maxLct, -est + maxLct, processingTimes(i), heights(i))
    }

    positiveInstance = Instance(tasks, capacity)
    negativeInstance = Instance(negativeTasks, capacity)

    filteringAlgorithm = new BinarySearchPropagator(CumulativeArguments(
      virtualCache = new VirtualInitialisationCache(arity * arity * 9)
    ))
    filteringAlgorithm.initialize(positiveInstance, negativeInstance)

    ps
  }

  def revise(ps: ProblemState, mod: BitVector): Outcome = {
    var maxLct = Int.MinValue
    for (i <- 0 until nbTasks) {
      val stDom = ps.dom(startingTimes(i))
      val lct = stDom.last + processingTimes(i)
      tasks(i).est = stDom.head
      tasks(i).lct = lct
      maxLct = Math.max(maxLct, lct)
    }
    for (i <- 0 until nbTasks) {
      val est = tasks(i).est // startingTimes(i).getLB
      val lct = tasks(i).lct //startingTimes(i).getUB + processingTimes(i)
      negativeTasks(i).est = maxLct - lct
      negativeTasks(i).lct = maxLct - est
    }
    filteringAlgorithm.update()

    if (!filteringAlgorithm.isConsistent) {
      Contradiction(scope)
    } else {
      //      BinarySearchChecker.reset()
      val bounds = filteringAlgorithm.filter()
      if (bounds == null || bounds.est == null || bounds.lct == null) {
        Contradiction(scope)
      } else {
        ps.fold(bounds.est.indices) { (s, i) =>
          s.shaveDom(startingTimes(i), bounds.est(i), bounds.lct(i) - processingTimes(i))
        }
      }

    }

    //    }
  }

  def nbTasks: Int = startingTimes.length

  override def simpleEvaluation: Int = 3

  override protected def advise(problemState: ProblemState, event: Event, pos: Int): Int = {
    arity * arity
  }
}
