package concrete.constraint.semantic

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import concrete.InsideRemoval
import concrete.IntDomain
import concrete.Problem
import concrete.Singleton
import concrete.Variable
import concrete.constraint.AdviseCount
import concrete.Assignment

/**
 * @author vion
 */
class ElementTest extends FlatSpec with Matchers {

  "Element constraint" should "not create new domains when NOP" in {
    val r = new Variable("r", IntDomain.ofInterval(6, 14))
    val i = new Variable("i", IntDomain.ofSeq(2, 4, 15))
    val a2 = new Variable("a2", IntDomain.ofSeq(3, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
  }

  it should "filter correctly test case" in {
    val r = new Variable("r", IntDomain.ofSeq(47, 59, 65))
    val i = new Variable("i", IntDomain.ofSeq(10, 13))
    val v10 = new Variable("v10", IntDomain.ofSeq(65))
    val v13 = new Variable("v13", IntDomain.ofSeq(59, 65))
    val c = new ElementWatch(r, i, Array(null, null, null, null, null, null, null, null, null, null, v10, null, null, v13, v13))

    c.resultWatches ++= Map(59 -> 13, 65 -> 10, 47 -> 13)
    c.watched(10) = 1
    c.watched(13) = 2

    val problem = Problem(r, i, v10, v13)
    problem.addConstraint(c)
    c.register(new AdviseCount)

    val state = problem.initState.toState

    c.adviseArray(state, InsideRemoval, Array(3, 4))
    val mod = c.revise(state)
    mod.dom(r).view should contain theSameElementsAs Seq(59, 65)
    //th of ArrayBuffer({}, [47], [59], [65], [65], [65], [65], [65], [65], [65], [65], [65], [65], {59, 65}, {59, 65}, {47, 59, 65}) -> NOP

  }

  it should "filter correctly second test case" in {
    val x234 = new Variable("x234", IntDomain.ofSeq(2, 5, 12, 17))
    val array = Array(7, 17, 19, 7, 2, 3, 20, 14, 19, 7, 14, 17, 3, 2, 14, 7, 20, 14, 9, 20)
      .zipWithIndex
      .map { case (d, i) => new Variable(s"v$i", Singleton(d)) }

    val c = new ElementRI(x234, null +: array)

    // revising X_INTRODUCED_234 {2, 5, 12, 17} =AC= X_INTRODUCED_234 {2, 5, 12, 17}th of ArrayBuffer({}, X_INTRODUCED_1424[22]||X_INTRODUCED_70||p[1] [7], X_INTRODUCED_1424[23]||X_INTRODUCED_71||p[2] [17], X_INTRODUCED_1424[24]||X_INTRODUCED_72||p[3] [19], X_INTRODUCED_1424[25]||X_INTRODUCED_73||p[4] [7], X_INTRODUCED_1424[26]||X_INTRODUCED_74||p[5] [2], X_INTRODUCED_1424[27]||X_INTRODUCED_75||p[6] [3], X_INTRODUCED_1424[28]||X_INTRODUCED_76||p[7] [20], X_INTRODUCED_1424[29]||X_INTRODUCED_77||p[8] [14], X_INTRODUCED_1424[30]||X_INTRODUCED_78||p[9] [19], X_INTRODUCED_1424[31]||X_INTRODUCED_79||p[10] [7], X_INTRODUCED_1424[32]||X_INTRODUCED_80||p[11] [14], X_INTRODUCED_1424[33]||X_INTRODUCED_81||p[12] [17], X_INTRODUCED_1424[34]||X_INTRODUCED_82||p[13] [3], X_INTRODUCED_1424[35]||X_INTRODUCED_83||p[14] [2], X_INTRODUCED_1424[36]||X_INTRODUCED_84||p[15] [14], X_INTRODUCED_1424[37]||X_INTRODUCED_85||p[16] [7], X_INTRODUCED_1424[38]||X_INTRODUCED_86||p[17] [20], X_INTRODUCED_1424[39]||X_INTRODUCED_87||p[18] [14], X_INTRODUCED_1424[40]||X_INTRODUCED_88||p[19] [9], X_INTRODUCED_1424[41]||X_INTRODUCED_89||p[20] [20])
    // constraint 193 has mod SmallBitVector{18}, watches  and 

    val problem = new Problem(x234 +: array)
    problem.addConstraint(c)
    val state = problem.initState.toState
    c.advise(state, Assignment, 18)
    c.revise(state)
      .andThen { mod =>
        fail(c.toString(mod))
      }

    // X_INTRODUCED_1424[38]||X_INTRODUCED_86||p[17] {1, 3, 4, 5, [12...], 20},

  }

}