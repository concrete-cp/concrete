package concrete.generator.cspompatterns

import cspom.CSPOM
import cspom.compiler._
import org.scalatest.concurrent.TimeLimits
import org.scalatest.{FlatSpec, Matchers, TryValues}

class MergeSameTest extends FlatSpec with Matchers with TimeLimits with TryValues {

//  "MergeSame" should "not take too long to compile" in {
//    val url = getClass.getResource("tsp-20-1_ext.xml")
//    CSPOM.load(url).map(cspom => failAfter(Span(10, Seconds)) {
//      CSPOMCompiler.compile(cspom, StandardCompilers() ++ StandardCompilers.improve())
//    }) should be a 'success
//  }

  "MergeSame" should "not alter the problem" in {
    val url = Option(getClass.getResource("Queens-0008-m1.xml.xz")).get
    CSPOM.load(url).map(cspom => CSPOMCompiler.compile(cspom, Seq(MergeSame, MergeEq, RemoveUselessEq))) should be a 'success

  }
}