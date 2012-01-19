package cspfj.util
import scala.collection.immutable.BitSet
import org.junit.Test
import scala.util.Random
import java.io.File
import java.io.PrintStream
import org.junit.Assert._

class HasseTest {
  val rand = new Random(0)

  def randSet(pool: Int, size: Int) = {
    BitSet.empty ++ Stream.continually(rand.nextInt(pool)).take(1 + rand.nextInt(size))
  }

  @Test
  def test() {

    val testSet = Stream.continually(randSet(100, 100)).take(200)

    val h = testSet.foldLeft(
      Hasse.empty(new SetInclusion[Int])) { (h, s) =>
        println(s);
        h + s
      }

    for ((s, c) <- h) {
      val countUB = testSet.count { ss => ss.subsetOf(s) }
      val countLB = testSet.distinct.count { ss => ss.subsetOf(s) }
      assertTrue("Set " + s, countLB <= c && c <= countUB)
    }

    //    h = List(
    //      BitSet(0, 1, 5),
    //      BitSet(1, 2, 5),
    //      BitSet(5),
    //      //BitSet(2, 5),
    //      //BitSet(1),
    //      BitSet(0, 1, 5)
    //      //BitSet(0, 5)
    //      ).foldLeft(Hasse.empty(new SetInclusion[Int]))((h, s) => h + s)
    //
    //    val f = new File("/tmp/hasse.gml")
    //    val ps = new PrintStream(f)
    //    ps.print(h.toGML)
    //    ps.close()
    //    launchYEd(f)
  }

  private def findYEd = {
    val path = System.getenv("PATH");
    ((System.getenv("HOME") + "/bin") :: path.split(":").toList).find { p =>
      println(p); new File(p + "/yEd").exists
    }
  }

  private def launchYEd(file: File) {
    val yEdPath = findYEd
    if (yEdPath.isDefined) {

      Runtime.getRuntime().exec(
        yEdPath.get + "/yEd " + file.getAbsolutePath());

    }

  }
}