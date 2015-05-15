package concrete.constraint.extension;

import org.scalatest.Finders
import org.scalatest.FlatSpec

import concrete.IntDomain
import concrete.Problem
import concrete.ProblemState
import concrete.Variable
import concrete.constraint.AdviseCount

final class ExtensionConstraint2DTest extends FlatSpec {

  val var1 = new Variable("V1", IntDomain(1 to 3))
  val var2 = new Variable("V2", IntDomain(1 to 4))

  val matrix2d = new Matrix2D(3, 4, 1, 1, false);
  matrix2d.set(Array(1, 1), true);
  matrix2d.set(Array(1, 3), true);

  val c = BinaryExt(Array(var1, var2), matrix2d, false)

  val ps = Problem(var1, var2).initState.toState

  "ExtensionConstraint2D" should "find supports" in {
    assert(c.hasSupport(ps, 0, 1))
    assert(!c.hasSupport(ps, 0, 2))
    assert(!c.hasSupport(ps, 0, 3))

    assert(c.hasSupport(ps, 1, 1))
    assert(!c.hasSupport(ps, 1, 2))
    assert(c.hasSupport(ps, 1, 3))
    assert(!c.hasSupport(ps, 1, 4))
  }

  it should "check correctly" in {
    assert(!c.check(Array(1, 2)))
    assert(c.check(Array(1, 1)))
  }

  it should "filter correctly" in {
    val v8 = new Variable("V8", IntDomain(0 to 16))
    val v16 = new Variable("V16", IntDomain(0 to 16))

    val ps = Problem(v8, v16)
      .initState
      .filterDom(v8)(v => Set(0, 2, 9, 15).contains(v))
      .filterDom(v16)(v => Set(5, 7, 8).contains(v))
      .asInstanceOf[ProblemState]

    val ac = new AdviseCount

    val nogoods2 = "0 2|0 5|0 11|0 16|1 1|1 10|1 12|1 13|1 14|2 1|2 3|2 5|2 9|2 11|2 12|2 16|3 4|3 8|3 12|4 1|4 2|4 4|4 5|4 7|5 0|5 4|5 5|5 14|5 16|6 6|6 11|7 2|7 6|7 13|7 14|7 15|8 6|8 7|8 8|8 13|9 0|9 5|9 6|9 10|10 0|10 1|10 2|10 8|10 10|10 11|11 10|12 5|12 7|12 8|12 13|12 15|13 1|13 11|14 0|14 2|14 9|14 11|14 12|15 2|15 4|15 5|15 6|15 7|15 10|16 1|16 10|16 11"
      .split("\\|").map(_.split(" ").map(_.toInt).toSeq)

    val matrix2 = new Matrix2D(17, 17, 0, 0, true)
    matrix2.setAll(nogoods2, false)
    val c2 = new BinaryExtNR(Array(v8, v16), matrix2, false)

    c2.register(ac)
    c2.adviseAll(ps)

    val mod = c2.revise(ps).asInstanceOf[ProblemState]

    c2.adviseAll(mod)
    c2.revise(mod)
    //    c.advise(domains, 0)
    //    println(c.revise(mod, Unit))

  }

}
