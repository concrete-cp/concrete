package concrete.constraint.semantic

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import concrete.IntDomain
import concrete.Variable

/**
 * @author vion
 */
class ElementTest extends FlatSpec with Matchers {

  "Element constraint" should "not create new domains when NOP" in {
    val r = new Variable("r", IntDomain.ofInterval(6, 14))
    val i = new Variable("i", IntDomain.ofSeq(2, 4, 15))
    val a2 = new Variable("a2", IntDomain.ofSeq(3, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
  }

}