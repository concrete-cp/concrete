package concrete.constraint.extension;

import org.junit.Before
import org.junit.Test
import org.junit.Assert._
import concrete.IntDomain
import concrete.Variable;
import org.scalatest.FlatSpec

final class ExtensionConstraint2DTest extends FlatSpec {

  val var1 = new Variable("V1", IntDomain(1 to 3))
  val var2 = new Variable("V2", IntDomain(1 to 4))

  val matrix2d = new Matrix2D(3, 4, false);
  matrix2d.set(Array(1, 1), true);
  matrix2d.set(Array(1, 3), true);

  val c = BinaryExt(Array(var1, var2), matrix2d, false)

  val domains = IndexedSeq(var1.initDomain, var2.initDomain)

  "ExtensionConstraint2D" should "find supports" in {
    assert(c.hasSupport(domains, 0, 1))
    assert(!c.hasSupport(domains, 0, 2))
    assert(!c.hasSupport(domains, 0, 3))

    assert(c.hasSupport(domains, 1, 1))
    assert(!c.hasSupport(domains, 1, 2))
    assert(c.hasSupport(domains, 1, 3))
    assert(!c.hasSupport(domains, 1, 4))
  }

  it should "check correctly" in {
    assert(!c.check(Array(1, 2)))
    assert(c.check(Array(1, 1)))
  }

}
