package concrete.generator.constraint;

import concrete.constraint.semantic.Mul
import concrete.generator.FailedGenerationException
import concrete.{ Variable, Problem, IntDomain }
import cspom.CSPOMConstraint
import concrete.UNSATObject
import concrete.constraint.Residues
import concrete.constraint.Constraint
import concrete.constraint.TupleEnumerator
import concrete.constraint.semantic.Sum

final class MulGenerator(problem: Problem) extends AbstractGenerator(problem) {

  override def genFunctional(constraint: CSPOMConstraint, r: C2Conc) = {
    val result = r.asInstanceOf[C21D]
    val Seq(v0, v1) = constraint.arguments map cspom2concrete1D

    if (Seq(result, v0, v1) collect { case C2V(v) if (v.dom.undefined) => v } match {
      case Seq() => true
      case Seq(v) if (result.is(v)) => {
        val values = AbstractGenerator.domainFrom(Seq(v0, v1), { case Seq(i, j) => i * j })
        v.dom = IntDomain(values: _*)
        true
      }
      case Seq(v) if (v0.is(v)) => {
        v.dom = IntDomain(generateValues(result, v1): _*)
        true
      }
      case Seq(v) if (v1.is(v)) => {
        v.dom = IntDomain(generateValues(result, v0): _*)
        true
      }
      case _ => false

    }) {
      val constraint = (result, v0, v1) match {
        case (C2C(r), C2C(v0), C2C(v1)) => if (r == v0 * v1) None else throw UNSATObject
        case (C2C(r), C2V(v0), C2V(v1)) => Some(
          new Constraint(Array(v0, v1)) with Residues {
            def checkValues(t: Array[Int]) = r == t(0) * t(1)
            def simpleEvaluation = 1
            def getEvaluation = scope(0).dom.size + scope(1).dom.size
            def findSupport(pos: Int, idx: Int) = {
              val other = 1 - pos
              val value = scope(pos).dom.value(idx)
              if (r % value != 0) {
                None
              } else {
                val sought = r / value

                if (scope(other).dom.presentVal(sought)) {
                  val support = new Array[Int](2)
                  support(pos) = idx
                  support(other) = scope(other).dom.index(sought)
                  Some(support)
                } else {
                  None
                }

              }
            }
          })
        case (C2V(r), C2C(v0), C2V(v1)) => Some(
          new Sum(0, Array(-1, v0), Array(r, v1)))
        //          new Constraint(Array(r, v1)) with Residues with TupleEnumerator {
        //            def checkValues(t: Array[Int]) = t(0) == v0 * t(1)
        //          })
        case (C2V(r), C2V(v0), C2C(v1)) => Some(
          new Sum(0, Array(-1, v1), Array(r, v0)))
        //            
        //          new Constraint(Array(r, v0)) with Residues {
        //            def checkValues(t: Array[Int]) = t(0) == t(1) * v1
        //            def simpleEvaluation = 1
        //            def getEvaluation = scope(0).dom.size + scope(1).dom.size
        //            def findSupport(pos: Int, idx: Int) = {
        //              val value = scope(pos).dom.value(idx)
        //              pos match {
        //                case 0 =>
        //                  if (value % v1 == 0) {
        //                    val sought = value / v1
        //                    val index = scope(1).dom.index(sought)
        //                    if (scope(1).dom.present(index)) {
        //                      Some(Array(idx, index))
        //                    } else {
        //                      None
        //                    }
        //                  } else {
        //                    None
        //                  }
        //                case 1 =>
        //                  val sought = v1 * value
        //                  val index = scope(0).dom.index(sought)
        //                  if (scope(0).dom.present(index)) {
        //                    Some(Array(index, idx))
        //                  } else {
        //                    None
        //                  }
        //                case _ => throw new IllegalStateException
        //              }
        //            }
        //          })

        case (C2V(r), C2V(v0), C2V(v1)) => Some(new Mul(r, v0, v1))
      }

      constraint.foreach(addConstraint)

      true
    } else {
      false
    }

  }

  private def generateValues(result: C21D, variable: C21D) = {
    AbstractGenerator.domainFromFlat(Seq(result, variable), {
      case Seq(i, j) => if (i % j == 0) Seq(i / j) else Seq()
    })
    //    AbstractGenerator.makeDomain(
    //      for {
    //        i <- result.dom.values.toSeq
    //        j <- variable.dom.values.toSeq
    //        if (i % j == 0)
    //      } yield i / j)
  }

}
