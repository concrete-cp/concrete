package concrete.runner

import java.io.File
import java.io.PrintStream
import scala.Array.canBuildFrom
import scala.annotation.tailrec
import scala.util.Random
import concrete.IntDomain
import cspom.StatisticsManager
import concrete.UNSATException
import concrete.Variable
import concrete.constraint.extension.ReduceableExt
import concrete.constraint.extension.MDD
import concrete.constraint.extension.STR
import concrete.util.BitVectorSet
import concrete.util.DirectIndices
import concrete.constraint.extension.TupleTrieSet
import concrete.constraint.extension.MDDC
import concrete.constraint.extension.ExtensionConstraintGeneral
import concrete.constraint.extension.FindSupportExt
import concrete.constraint.extension.ExtensionConstraintSTR3

object TrieSize extends App {

  val RAND = new Random(0)
  def randTuple(d: Int, k: Int) = Array.fill(k)(RAND.nextInt(d))

  val nullOut = new PrintStream(new File("/dev/null"))

  //  for (tight <- .1 to 1 by .1) {
  //    val TUPLES = tight * math.pow(d, ARITY)
  //    var t = ArrayTrie.empty
  //    while (t.size < TUPLES) t += randTuple(d, ARITY)
  //
  //    val m = MDD(t.toSeq: _*)
  //
  //    println("%f, %.0f, %d, %d".format(tight, (d * ARITY * TUPLES), t.nodes, m.nodes))
  //  }

  //def logScale: Stream[Int] = Stream(1, 2, 5) #::: logScale.map(_ * 10)

  def appliable(structure: String, algo: String, arity: Int, d: Int, l: Double) = {

    var count2 = 50

    var mem1s = List[Long]()

    var nodes = List[Int]()

    var timeFirst = List[Long]()

    var times = List[Long]()

    var mems = List[Long]()

    var inits = List[Long]()

    while (count2 > 0) {

      print(s"$count2 ")

      val data = tuples(arity, d).filter(i => RAND.nextDouble() < l).toSeq

      var initTime = -System.nanoTime()

      val t = new TupleTrieSet(structure match {
        case "MDD" => MDD(data: _*)
        case "STR" => new STR() ++ data
      }, false)
      initTime += System.nanoTime()

      inits ::= initTime

      val domains = Array.fill(arity)(
        new IntDomain(new BitVectorSet(d), new DirectIndices(d)))

      val variables = domains.map(new Variable(count2.toString, _))

      val constraint = algo match {
        case "MDDC" => {
          new MDDC(variables, t.reduceable.asInstanceOf[MDD])
        }
        case "STR3" => {
          new ExtensionConstraintSTR3(variables, t.reduceable.asInstanceOf[STR].array)
        }
        case "Reduce" => {
          new ReduceableExt(variables, t.reduceable.copy)
        }
        case "Find" => {
          new FindSupportExt(variables, t, true)
        }
        case "General" => {
          new ExtensionConstraintGeneral(t, true, variables)
        }
      }

      //Thread.sleep(500)
      System.gc()
      System.gc()
      System.gc()
      System.gc()
      System.gc()

      mem1s ::= Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()

      nodes ::= t.reduceable.edges

      var level = 0
      var time = -System.nanoTime()
      try {
        constraint.revise((arity - 1 to 0 by -1).toList)
      } catch {
        case _: UNSATException =>
      }

      time += System.nanoTime()
      timeFirst ::= time
      times ::= time

      try {
        var count = 19
        while (count >= 0) {
          level += 1
          constraint.setLvl(level)

          val modified = (0 until arity).toList.filter {
            p =>
              domains(p).filter(i => RAND.nextDouble > .05)
          } reverse

          //System.gc()
          time -= System.nanoTime()
          try {
            constraint.revise(modified)
          } finally {
            time += System.nanoTime()
            times ::= time
          }
          count -= 1
        }
      } catch {
        case _: UNSATException =>
      }
      System.gc()
      System.gc()
      System.gc()
      System.gc()
      System.gc()

      mems ::= Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
      nullOut.println(constraint.toString)
      count2 -= 1
    }

    println()

    Seq(StatisticsManager.median(inits), StatisticsManager.median(mem1s), StatisticsManager.median(nodes), StatisticsManager.median(mems), StatisticsManager.median(timeFirst), StatisticsManager.median(times))
  }

  def check(d: Array[IntDomain], p: Int, i: Int) = d(p).present(i)

  val algos = List(
    //   ("MDD", "Find"),
    ("MDD", "Reduce"),
    ("MDD", "MDDC"),
    ("STR", "Reduce"))

  for ((s, a) <- algos) {
    appliable(s, a, 5, 10, .5)
  }

  def name(s: Seq[Int], n: String) = s match {
    case Seq(v) => v.toString
    case _ => n
  }

  def bench(ks: Seq[Int], ls: Seq[Double], ds: Seq[Int]) {

    val fname = s"${name(ks, "k")}-${name(ls.map(l => (100 * l).toInt), "l")}-${name(ds, "d")}"

    println(fname)

    val f = new File(s"/home/vion/mdd/$fname.csv")
    if (f.exists()) {
      println("skip")
    } else {

      val out = new PrintStream(new File(s"/home/vion/mdd/$fname.csv"))

      try {
        out.print("k,l,d,")

        out.println(
          algos.flatMap {
            case (s, a) => List("Init", "Mem", "Nodes", "Mem2", "Time1", "Time").map(s + a + _)
          } mkString (","))

        for (
          d <- ds;
          l <- ls; //0.0 to 1 by .02; 
          k <- ks
        ) {

          print(s"$k-$l-$d :")

          out.println(
            s"$k,$l,$d," + (algos.map {
              case (s, a) => appliable(s, a, k, d, l).mkString(",")
            } mkString (",")))

        }
      } catch {
        case e: Throwable => e.printStackTrace(System.err)
      } finally {
        out.close()
      }
    }
  }

  bench(Seq(5), Seq(.5), 2 to 50)
  bench(Seq(5), 0.02 to 1.0 by .02, Seq(10))
  bench(2 to 15, Seq(.5), Seq(10))
  //
  //  bench(Seq(6), Seq(.1), 2 to 50)
  //  bench(Seq(6), 0.02 to 1.0 by .02, Seq(10))
  //  bench(2 to 15, Seq(.1), Seq(10))
  //
  //  bench(Seq(10), Seq(.01), 2 to 50)
  //  bench(Seq(10), 0.02 to 1.0 by .02, Seq(5))
  //  bench(2 to 20, Seq(.01), Seq(5))

  nullOut.close()
  //println(t.mdds.size)
  //println(t.iterator.map(_.mkString(" ")).mkString("\n"))
  //  t = t.filterTrie({
  //    case (depth, i) => depth != 0 || i == 0
  //  }, (0 until 8).toList, 0)
  //  println(t)

  private def firstTuple(arity: Int): Array[Int] = Array.fill(arity)(0)

  @tailrec
  private def nextTuple(t: Array[Int], d: Int, p: Int): Array[Int] =
    if (p < 0) null
    else {
      val t2 = t.clone
      val index = t2(p) + 1
      if (index >= d) {
        t2(p) = 0
        nextTuple(t2, d, p - 1)
      } else {
        t2(p) = index
        t2
      }
    }

  def tuples(arity: Int, d: Int) = new Iterator[Array[Int]] {
    var current = firstTuple(arity)

    def hasNext = current ne null

    def next() = {
      val c = current
      current = nextTuple(current, d, arity - 1)
      c
    }
  }

}
