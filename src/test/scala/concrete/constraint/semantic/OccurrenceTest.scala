package concrete.constraint.semantic
import org.junit.Test
import concrete.IntDomain
import concrete.Variable
import concrete.UNSATException
import org.junit.Assert._
import org.hamcrest.CoreMatchers._

class OccurrenceTest {
  @Test
  def test() {
    val v1 = new Variable("1", IntDomain(7))
    val v2 = new Variable("2", IntDomain(6))
    val v3 = new Variable("3", IntDomain(7, 9))
    val v4 = new Variable("4", IntDomain(8))
    val v5 = new Variable("5", IntDomain(8, 9))

    val occ = new Variable("occ", IntDomain(1, 2, 3))

    val c = new OccurrenceVar(occ, 7, Array(v1, v2, v3, v4, v5))

    assertEquals(Seq(0), c.revise())

    assertEquals(List(1, 2), occ.dom.values.toList)

  }

  @Test(expected = classOf[UNSATException])
  def test2() {
    val v1 = new Variable("1", IntDomain(7))
    val v2 = new Variable("2", IntDomain(6))
    val v3 = new Variable("3", IntDomain(7, 9))
    val v4 = new Variable("4", IntDomain(8))
    val v5 = new Variable("5", IntDomain(8, 9))

    val occ = new Variable("occ", IntDomain(3, 4, 5))

    val c = new OccurrenceVar(occ, 7, Array(v1, v2, v3, v4, v5))
    c.revise()

  }

}