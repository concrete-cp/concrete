package concrete.heuristic.value

import java.util.EventObject

import concrete._
import concrete.heuristic.branch.BranchHeuristic
import concrete.heuristic._

import scala.util.{Random, Try}

object ValueSelector {
  def default(params: ParameterManager, rand: Random): Try[Seq[ValueSelector]] = {
    params.getRaw("heuristic.value")
      .map {
        case h: Seq[_] => instantiate(params, h, rand)
        case s: String => instantiate(params, s.split(","), rand)
        case c: Any => instantiate(params, Seq(c), rand)
      }
      .getOrElse(instantiate(params, default, rand))
  }

  def default: Seq[Class[_ <: ValueSelector]] = {
    Seq(classOf[RandomBoundDiv], classOf[BestValue], classOf[PhaseSaving], classOf[BestCost],
      classOf[RandomBound])
  }

  def instantiate(pm: ParameterManager, seq: Seq[Any], rand: Random): Try[Seq[ValueSelector]] =
    Try {
      seq.map {
        case h: ValueSelector => h
        case s: Class[ValueSelector] => ValueSelector(pm, s, rand).get
        case s: String =>
          ValueSelector(
            pm,
            ParameterManager.classInPackage(s, "concrete.heuristic.value").asInstanceOf[Class[ValueSelector]],
            rand).get
      }
    }

  def apply(pm: ParameterManager, valueSelectorClass: Class[_ <: ValueSelector],
            rand: Random): Try[ValueSelector] = {

    Try {
      valueSelectorClass.getConstructor().newInstance()
    }
      .recover {
        case _: NoSuchMethodException =>
          valueSelectorClass.getConstructor(classOf[ParameterManager])
            .newInstance(pm)
      }
      .recover {
        case _: NoSuchMethodException =>
          valueSelectorClass.getConstructor(classOf[Random])
            .newInstance(rand)
      }
      .recover {
        case _: NoSuchMethodException =>
          valueSelectorClass.getConstructor(classOf[ParameterManager], classOf[Random])
            .newInstance(pm, rand)
      }
  }
}


trait ValueSelector {
  def select(ps: ProblemState, variable: Variable, candidates: Domain): (Outcome, Domain)

  def compute(solver: MAC, ps: ProblemState): ProblemState

  def shouldRestart: Boolean

  def event[S <: Outcome](e: EventObject, ps: S): S = ps
}


class ValueHeuristic(selectors: ValueSelector*) extends BranchHeuristic {

  def this(pm: ParameterManager, rand: Random) = {
    this(ValueSelector.default(pm, rand).get: _*)
  }


  def branch(variable: Variable, domain: Domain, ps: ProblemState): Either[Contradiction, (ProblemState, Decision, Decision)] = {
    val it = selectors.iterator

    def selectOne(ps: Outcome, candidates: Domain): Either[Contradiction, (ProblemState, Int)] = {
      ps match {
        case s: ProblemState if it.hasNext && candidates.size > 1 =>
          (selectOne _).tupled(it.next.select(s, variable, candidates))
        case s: ProblemState => Right((s, candidates.head))
        case c: Contradiction => Left(c)
      }
    }

    selectOne(ps, domain) match {
      case Right((s, selected)) =>
        val d = s.dom(variable)
        if (d.isAssigned) {
          Right((s, Continue, DeadEnd()))
        } else {
          Right((s, Assign(variable, selected), Remove(variable, selected)))
        }
      case Left(c) => Left(c)
    }
  }

  override def toString: String = "selectors: " + selectors.mkString(", ")

  def compute(solver: MAC, ps: ProblemState): ProblemState = selectors.foldRight(ps)(_.compute(solver, _))

  override def shouldRestart: Boolean = selectors.exists(_.shouldRestart)

  override def event[S <: Outcome](e: EventObject, ps: S): S = selectors.foldRight(ps)(_.event(e, _))
}
