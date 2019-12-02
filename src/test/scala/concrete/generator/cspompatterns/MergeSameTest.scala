package concrete.generator.cspompatterns

import java.io.FileNotFoundException

import cspom.CSPOM
import cspom.compiler._
import org.scalatest.concurrent.TimeLimits
import org.scalatest.TryValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MergeSameTest extends AnyFlatSpec with Matchers with TimeLimits with TryValues {

//  "MergeSame" should "not take too long to compile" in {
//    val url = getClass.getResource("tsp-20-1_ext.xml")
//    CSPOM.load(url).map(cspom => failAfter(Span(10, Seconds)) {
//      CSPOMCompiler.compile(cspom, StandardCompilers() ++ StandardCompilers.improve())
//    }) should be a 'success
//  }

  "MergeSame" should "not alter the problem" in {
    val file = "Queens-0008-m1.xml.xz"
    val url = Option(getClass.getResource(file))
      .getOrElse(throw new FileNotFoundException(s"Could not find $file"))
    assert(
      CSPOM.load(url)
        .map(cspom => CSPOMCompiler.compile(cspom, Seq(MergeSame, MergeEq, RemoveUselessEq)))
        .isSuccess
    )

  }
}