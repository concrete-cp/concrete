package concrete.constraint.semantic


import bitvectors.BitVector
import concrete._
import concrete.constraint.Constraint
import concrete.heuristic.{Assign, Remove}
import concrete.util.BitSetQueue
import cspom.Statistic

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class NoGoods(scope: Array[Variable]) extends Constraint(scope) {

  private val nogoods: mutable.Set[NoGood] = new mutable.HashSet()
  private val watched1: Array[mutable.Set[NoGood]] = Array.fill(arity)(new mutable.HashSet())
  private val watched2: Array[mutable.Set[NoGood]] = Array.fill(arity)(new mutable.HashSet())
  private val uninitialized = new ArrayBuffer[NoGood]()
  var threshold: Int = 200000
  @Statistic
  var nbNogoods = 0
  @Statistic
  var nbInferences = 0
  @Statistic
  var maxNoGoods = 0
  @Statistic
  var currentNoGoods: Int = 0

  var clearNb = 0

  override def toString(ps: ProblemState) = super.toString(ps) // + "\n" + nogoods.map(_.toString(ps)).mkString("\n")

  override def revise(initState: ProblemState, modified: BitVector): Outcome = {
    // println(s"mod: $modified")
    var state = initState

    val queue = new BitSetQueue(modified.words)


    logger.trace("Checking uninitialized")
    for (ng <- uninitialized) {
      ng.findWatch(state) match {
        case None =>
          logger.warn(s"Unsat nogood ${ng.toString(state)}")
          return Contradiction(ng.variables)
        case Some((watch1, witness1)) =>
          ng.findWatch(state, from = watch1 + 1) match {
            case None =>
              ng.infer(state, watch1) match {
                case c: Contradiction =>
                  logger.warn(s"Unsat nogood after inference ${ng.toString(state)}")
                  return c
                case ps: ProblemState =>
                  queue.offer(ng.literals(watch1).pos)
                  logger.debug(s"Inferrable nogood ${ng.toString(state)}")
                  state = ps
              }
            case Some((watch2, witness2)) =>
              ng.watch1 = watch1
              ng.witness1 = witness1
              ng.watch2 = watch2
              ng.witness2 = witness2
              watched1(ng.lit1.pos) += ng
              watched2(ng.lit2.pos) += ng
              nogoods += ng
              nbNogoods += 1
              currentNoGoods += 1
              maxNoGoods = math.max(maxNoGoods, nogoods.size)
          }
      }
    }

    uninitialized.clear()

    while(!queue.isEmpty) {
      val pos = queue.poll()
      logger.trace(s"Modified $pos: ${scope(pos)}")
      val dom = state.dom(scope(pos))
      logger.trace(s"Checking watches1")

      // BEWARE OF CONCURRENT MODIFICATIONS! Hence the .toSeq.

      for (ng <- watched1(pos).toSeq) {
        logger.trace(ng.toString(state))
        assert(ng.constraintPos(ng.watch1) == pos)

        val s2 = checkW1(ng, dom, state)

        if (s2 ne state) {
          upd(ng, state, s2)

          s2 match {
            case c: Contradiction => return c
            case s: ProblemState =>
              queue.offer(ng.lit2.pos)
              // logger.debug(diff(state, s).toString)
              state = s
          }
        }
      }

      logger.trace(s"Checking watches2")
      for (ng <- watched2(pos).toSeq) {
        logger.trace(ng.toString(state))
        assert(watched1(ng.lit1.pos).contains(ng))
        assert(ng.lit2.pos == pos)

        val s2 = checkW2(ng, dom, state)

        if (s2 ne state) {
          upd(ng, state, s2)

          s2 match {
            case c: Contradiction => return c
            case s: ProblemState =>
              queue.offer(ng.lit1.pos)
              // logger.trace(diff(state, s).toString)
              state = s
          }
        }

      }

    }


    state
  }

  private def upd(ng: NoGood, state: ProblemState, s2: Outcome): Unit = {
    nbInferences += 1
    ng.activity = clearNb
    logger.debug(s"${ng.toString(state)} triggered " + (
      if (s2.isState) {
        val dif = diff(state, s2.toState)
        assert(dif.forall { case (_, (d1, d2)) => d2.subsetOf(d1) }, s"${ng.toString(state)} augmented domain in $dif")
        dif.mkString(", ")
      } else {
        s2.toString
      }
      ))
  }

  def checkW1(ng: NoGood, dom: Domain, state: ProblemState): Outcome = {
    if (dom.contains(ng.witness1)) {
      assert(ng.possible(dom, ng.lit1).isDefined)
      state
    } else {
      ng.possible(dom, ng.lit1) match {
        case None =>
          ng.findWatch(state, ng.watch2) match {
            case None => ng.infer(state, ng.watch2)
            case Some((newWatch, newWitness)) =>
              watched1(ng.lit1.pos) -= ng
              ng.watch1 = newWatch
              ng.witness1 = newWitness
              watched1(ng.lit1.pos) += ng
              state
          }
        case Some(newWitness) =>
          ng.witness1 = newWitness
          state
      }
    }
  }


  def checkW2(ng: NoGood, dom: Domain, state: ProblemState): Outcome = {
    if (dom.contains(ng.witness2)) {
      assert(ng.possible(dom, ng.lit2).isDefined)
      state
    } else {
      ng.possible(dom, ng.lit2) match {
        case None =>
          ng.findWatch(state, ng.watch1) match {
            case None => ng.infer(state, ng.watch1)
            case Some((newWatch, newWitness)) =>
              watched2(ng.lit2.pos) -= ng
              ng.watch2 = newWatch
              ng.witness2 = newWitness
              watched2(ng.lit2.pos) += ng
              state
          }
        case Some(newWitness) =>
          ng.witness2 = newWitness
          state
      }
    }
  }

  override def init(ps: ProblemState): ProblemState = ps

  /**
    * @return true iff the constraint is satisfied by the given tuple
    */
  override def check(tuple: Array[Int]): Boolean = {
    val invalid = (nogoods.toSet ++ uninitialized).filter(ng => ng.matches(tuple))
    invalid.foreach { ng =>
      logger.warn(s"$ng was not propagated")
      logger.warn("tuple : " + ng.literals.map(l => tuple(l.pos)).mkString(","))
      if (ng.watch1 < 0) {
        logger.warn("uninitialized")
      } else {
        logger.warn(s"watch1: ${watched1(ng.constraintPos(ng.watch1))(ng)}")
        logger.warn(s"watch2: ${watched2(ng.constraintPos(ng.watch2))(ng)}")
      }
    }
    invalid.isEmpty
    //!nogoods.exists(ng => ng.matches(tuple))
  }

  override def simpleEvaluation = 2

  def addNoGood(implicants: Seq[heuristic.Assign], implies: heuristic.Remove): NoGood = {
    val ngImplicants = implicants.map {
      case Assign(variable, value) =>
        val Array(pos) = position(variable)
        Alternative(pos, neg = true, Singleton(value))
    }

    val implicand = {
      val Remove(variable, value) = implies
      val Array(pos) = position(variable)
      Alternative(pos, neg = true, Singleton(value))
    }

    val ng = NoGood((implicand +: ngImplicants).toArray)
    uninitialized += ng
    logger.debug("new nogood " + ng)
    ng
  }

  def addNoGood(implicants: Seq[heuristic.Assign], impVariable: Variable, impDom: Domain): Option[NoGood] = {
    if (uninitialized.size < threshold) {
      val ngImplicants = implicants.map {
        case Assign(variable, value) =>
          val Array(pos) = position(variable)
          Alternative(pos, neg = true, Singleton(value))
      }

      val Array(pos) = position(impVariable)
      val implicand = Alternative(pos, neg = false, impDom)

      val ng = NoGood((ngImplicants :+ implicand).toArray)
      uninitialized += ng
      logger.debug("new nogood " + ng)
      Some(ng)
    } else {
      None
    }
  }

  def clearInactive(): Unit = {
    clearNb += 1
    // Remove old, long nogoods. Sort them with "worst" nogoods first.
    val sorted = nogoods.toSeq.sortBy(ng => ng.literals.length * (ng.activity - clearNb))

    // threshold *= 1.1
    for (ng <- sorted.take(nogoods.size - threshold)) {
      logger.debug(s"nogood removed : length ${ng.literals.length}, age ${clearNb - ng.activity}")
      nogoods -= ng
      currentNoGoods -= 1
      watched1(ng.constraintPos(ng.watch1)) -= ng
      watched2(ng.constraintPos(ng.watch2)) -= ng
    }

  }


  override protected def advise(problemState: ProblemState, event: Event, pos: Int): Int = {
    arity * (watched1(pos).size + watched2(pos).size + uninitialized.size)
  }


  case class Alternative(pos: Int, neg: Boolean, dom: Domain) {
    override def toString: String = s"${scope(pos)} ${if (neg) "notin" else "in"} $dom"

    def toString(ps: ProblemState): String = s"${scope(pos).toString(ps)} ${if (neg) "notin" else "in"} $dom"
  }

  class NoGood private(val literals: Array[Alternative], val id: Int) {

    var activity: Int = clearNb

    /** watch < 0 means that all variables must be watched */
    var watch1: Int = -1
    var watch2: Int = -1
    var witness1: Int = _
    var witness2: Int = _

    def lit1 = literals(watch1)

    def lit2 = literals(watch2)

    override def toString: String = literals.mkString("(", ", ", ")") + s" - w1 = $watch1 ($witness1) - w2 = $watch2 ($witness2) - a = $activity"

    def toString(ps: ProblemState): String = literals.map(_.toString(ps)).mkString("(", ", ", ")") + s" - w1 = $watch1/${lit1.pos} ($witness1) - w2 = $watch2/${lit2.pos} ($witness2) - a = $activity"

    def findWatch(ps: ProblemState, otherThan: Int = -1, from: Int = 0): Option[(Int, Int)] = {
      var i = from
      while (i < literals.length) {
        if (i != otherThan) {
          val lit = literals(i)
          val pos = possible(ps.dom(scope(lit.pos)), lit)
          if (pos.isDefined) return Some((i, pos.get))
        }
        i += 1
      }
      None
    }

    /** Returns Some(witness) that dom in/notin lit.dom is possible, or else None */
    def possible(dom: Domain, lit: Alternative): Option[Int] = {
      if (lit.neg) {
        // should detect when var notin dom is possible
        (dom -- lit.dom).headOption
      } else {
        // should detect when var in dom is possible
        (dom & lit.dom).headOption
      }
    }

    /** Returns None if dom in/notin lit.dom is mandatory, or else Some(witness) that it is not mandatory */
    def mandatory(dom: Domain, lit: Alternative): Option[Int] = {
      if (lit.neg) {
        // detects when var notin dom is mandatory
        (dom & lit.dom).headOption
      } else {
        // detects when var in dom is mandatory
        (dom -- lit.dom).headOption
      }
    }

    override def hashCode: Int = id

    override def equals(o: Any): Boolean = o match {
      case ng: NoGood => ng.id == id
      case _ => false
    }

    def watched: Seq[Alternative] = if (watch1 < 0 || watch2 < 0) literals else Seq(literals(watch1), literals(watch2))

    def matches(tuple: Array[Int]): Boolean =
      literals.forall { decision =>
        !decision.neg ^ decision.dom.contains(tuple(decision.pos))
      }

    def infer(ps: ProblemState, pos: Int): Outcome = {
      if (pos < 0) {
        Contradiction(variables)
      } else {
        val lit = literals(pos)
        if (lit.neg) {
          // var notin dom is mandatory, so remove all values from dom
          ps.filterDom(scope(lit.pos))(v => !lit.dom.contains(v))
        } else {
          // var in dom is mandatory
          ps.intersectDom(scope(lit.pos), lit.dom)
        }
      }
    }

    def variables: Seq[Variable] = literals.indices.map(variable)

    def variable(pos: Int): Variable = scope(constraintPos(pos))

    def constraintPos(i: Int): Int = literals(i).pos

  }

  object NoGood {
    var ids = 0

    def apply(literals: Array[Alternative]): NoGood = {
      val ng = new NoGood(literals, ids)
      ids += 1
      ng
    }
  }

}