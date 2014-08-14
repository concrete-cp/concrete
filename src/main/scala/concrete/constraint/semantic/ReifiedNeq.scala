package concrete.constraint.semantic

import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.TupleEnumerator
import concrete.constraint.Residues
import concrete.TRUE
import concrete.FALSE
import concrete.UNKNOWNBoolean
import concrete.BooleanDomain
import concrete.EMPTY

class ReifiedNeq(val b: Variable, val v: Variable, val c: Int)
  extends Constraint(Array(b, v)) {

  val controlDomain = b.dom.asInstanceOf[BooleanDomain]

  def checkValues(t: Array[Int]) = t(0) match {
    case 0 => t(1) == c
    case 1 => t(1) != c
    case _ => sys.error(s"$b must be boolean")
  }

  override def toString = s"$b = $v != $c"

   val cIndex = v.dom.index(c)

  def revise() = {
    var ch = List[Int]()
    controlDomain.status match {
      case UNKNOWNBoolean =>
        if (v.dom.present(cIndex)) {
          if (v.dom.size == 1) {
            controlDomain.setFalse()
            entail()
            ch ::= 0
          }
        } else {
          controlDomain.setTrue()
          entail()
          ch ::= 0
        }

      case FALSE =>
        if (v.dom.assign(cIndex)) {
          ch ::= 1
        }
        entail()

      case TRUE =>
        if (v.dom.present(cIndex)) {
          v.dom.remove(cIndex)
          ch ::= 1
        }
        entail()

      case EMPTY => throw new AssertionError()
    }

    ch
  }

  def advise(pos:Int) = 1
  def simpleEvaluation: Int = 1
  
}