package concrete
package constraint
package semantic

import scala.util.Random

import com.github.davidmoten.rtree.RTree
import com.github.davidmoten.rtree.geometry.Geometry
import com.github.davidmoten.rtree.geometry.Rectangle

import concrete.heuristic.Lexico
import cspom.util.BitVector
import cspom.CSPOM
import cspom.variable.IntVariable
import cspom.CSPOMConstraint
import cspom.variable.CSPOMExpression
import cspom.CSPOMGoal
import cspom.StatisticsManager
import com.github.davidmoten.rtree.Entry
import concrete.util.Interval
import scala.collection.mutable.HashMap
import cspom.Statistic

class IntRectangle(
    val x1: Int,
    val y1: Int,
    val x2: Int,
    val y2: Int) extends Geometry {

  assert(x2 >= x1)

  assert(y2 >= y1)

  override def intersects(r: com.github.davidmoten.rtree.geometry.Rectangle): Boolean = {
    r.x2 > x1 && r.x1 < x2 && r.y2 > y1 && r.y1 < y2
  }

  override def distance(r: Rectangle): Double =
    asRectangle.distance(r)

  override def toString(): String = {
    s"Rectangle [x1=$x1, y1=$y1, x2=$x2, y2=$y2]"
  }

  override def hashCode(): Int = (x1, y1, x2, y2).hashCode

  override def equals(obj: Any): Boolean = {
    obj match {
      case other: IntRectangle => x1 == other.x1 && x2 == other.x2 && y1 == other.y1 && y2 == other.y2
      case _ => false
    }
  }

  def mbr = asRectangle

  lazy val asRectangle: Rectangle = Rectangle.create(x1, y1, x2, y2)
}

class DiffNSpaceChecker(xs: Array[Variable], ys: Array[Variable], dxs: Array[Variable], dys: Array[Variable]) extends Constraint(xs ++ ys ++ dxs ++ dys) {
  def advise(ps: ProblemState, pos: Int) = xs.length + ys.length
  def check(tuple: Array[Int]): Boolean = ???
  def init(ps: ProblemState): Outcome = ps
  def revise(ps: ProblemState): Outcome = {
    val domX0 = ps.dom(xs(0))
    val domY0 = ps.dom(ys(0))
    val dx0 = ps.dom(dxs(0)).head
    val dy0 = ps.dom(dys(0)).head
    var xSpan = Interval(domX0.head, domX0.last + dx0)
    var ySpan = Interval(domY0.head, domY0.last + dy0)

    var minArea = dx0 * dy0

    for (i <- 1 until xs.length) {
      val domX = ps.dom(xs(i))
      val domY = ps.dom(ys(i))
      val dx = ps.dom(dxs(i)).head
      val dy = ps.dom(dys(i)).head

      xSpan = xSpan span Interval(domX.head, domX.last + dx)
      ySpan = ySpan span Interval(domY.head, domY.last + dy)
      minArea += dx * dy
    }

    // Do not use interval size as it counts discrete elements, not
    // interval length
    val availableArea = (xSpan.ub - xSpan.lb) * (ySpan.ub - ySpan.lb)

    if (availableArea < minArea) {
      Contradiction
    } else {
      ps
    }

  }
  def simpleEvaluation: Int = 2

}

class DiffN(xs: Array[Variable], ys: Array[Variable], dxs: Array[Variable], dys: Array[Variable]) extends Constraint(xs ++ ys ++ dxs ++ dys)
    with StatefulConstraint[(RTree[Int, IntRectangle], Vector[Option[Entry[Int, IntRectangle]]])] with Removals {
  def getEvaluation(problemState: ProblemState): Int = xs.length * xs.length
  def check(tuple: Array[Int]): Boolean = ???

  def init(ps: ProblemState): concrete.Outcome = {

    val map = Vector.tabulate(xs.length)(i => mandatory(ps, i).map(Entry.entry(i, _)))
    val tree = map.zipWithIndex.foldLeft(RTree.create[Int, IntRectangle]) {
      case (t, (r, i)) =>
        r.foldLeft(t) { (t, r) =>
          if (!t.search(r.geometry.asRectangle).isEmpty.toBlocking.single) {
            return Contradiction
          }
          t.add(r)
        }

    }

    ps.updateState(this, (tree, map))

  }

  def revise(ps: ProblemState, mod: BitVector): Outcome = {
    var (tree, map) = ps(this)

    {
      // Update RTree
      val xyRect = mod | mod.shift(-2 * xs.length)
      val modRectangles = xyRect | xyRect.shift(-xs.length)
      //println(s"modified $mod, so rectangles $modRectangles")

      var i = modRectangles.nextSetBit(0)

      while (i >= 0 && i < xs.length) {
        for (
          nr <- mandatory(ps, i) if !map(i).contains(nr)
        ) {
          for (r <- map(i)) tree = tree.delete(r)

          if (!tree.search(nr.asRectangle).isEmpty.toBlocking.single) {
            return Contradiction
          }

          val entry = Entry.entry(i, nr)
          map = map.updated(i, Some(entry))
          tree = tree.add(entry)

        }
        i = modRectangles.nextSetBit(i + 1)
      }

    }

    // Filter bounds until fixpoint
    var state = ps

    var lastModified = 0
    var j = 0

    do {

      val treeWoutMe = map(j).foldLeft(tree)(_ delete _)

      val domX = ps.dom(xs(j))
      val domY = ps.dom(ys(j))
      val dx = ps.dom(dxs(j)).head
      val dy = ps.dom(dys(j)).head

      val shrink = for {
        xlb <- filterXLeft(j, domX, domY, dx, dy, treeWoutMe)
        xub <- filterXRight(j, domX, domY, dx, dy, treeWoutMe)
        ylb <- filterYDown(j, domX, domY, dx, dy, treeWoutMe)
        yub <- filterYUp(j, domX, domY, dx, dy, treeWoutMe)
      } yield (Interval(xlb, xub), Interval(ylb, yub))

      shrink match {
        case None => return Contradiction
        case Some((newX, newY)) =>

          val ns = state
            .shaveDomNonEmpty(xs(j), newX)
            .shaveDomNonEmpty(ys(j), newY)
          if (ns ne state) {
            state = ns
            lastModified = j

            mandatory(ns, j) match {
              case Some(nr) =>
                DiffN.treeQueries += 1
                if (!treeWoutMe.search(nr.asRectangle).isEmpty.toBlocking.single) {
                  return Contradiction
                }

                val entry = Entry.entry(j, nr)

                tree = treeWoutMe.add(entry)
                map = map.updated(j, Some(entry))

              case None =>
                require(map(j).isEmpty)
              //map = map.updated(i, None)
            }

          }
      }

      j += 1

      if (j >= xs.length) { j = 0 }

    } while (j != lastModified)

    state.updateState(this, (tree, map))

  }

  //private val leftResidue = new HashMap[Int, Int]()
  private def filterXLeft(id: Int, domX: Domain, domY: Domain, dx: Int, dy: Int, tree: RTree[Int, IntRectangle]): Option[Int] = {
    var n = domX.head

    while (true) {
      findInColumn(n, id, dx, dy, domY, tree) match {
        case None => return Some(n)
        case Some((m, _)) =>
          domX.nextOption(m - 1) match {
            case Some(m) => n = m
            case None => return None
          }
      }
    }

    throw new AssertionError
  }

  private def filterXRight(id: Int, domX: Domain, domY: Domain, dx: Int, dy: Int, tree: RTree[Int, IntRectangle]): Option[Int] = {
    var n = domX.last
    while (true) {
      findInColumn(n, id, dx, dy, domY, tree) match {
        case None => return Some(n)
        case Some((_, m)) =>
          domX.prevOption(m - dx + 1) match {
            case Some(m) => n = m
            case None => return None
          }
      }
    }
    throw new AssertionError
  }

  private def filterYDown(id: Int, domX: Domain, domY: Domain, dx: Int, dy: Int, tree: RTree[Int, IntRectangle]): Option[Int] = {
    var n = domY.head

    while (true) {
      findInRow(n, id, dx, dy, domX, tree) match {
        case None => return Some(n)
        case Some((m, _)) =>
          domY.nextOption(m - 1) match {
            case Some(m) => n = m
            case None => return None
          }
      }
    }
    throw new AssertionError
  }
  private def filterYUp(id: Int, domX: Domain, domY: Domain, dx: Int, dy: Int, tree: RTree[Int, IntRectangle]): Option[Int] = {
    var n = domY.last

    while (true) {
      findInRow(n, id, dx, dy, domX, tree) match {
        case None => return Some(n)
        case Some((_, m)) =>
          domY.prevOption(m - dy + 1) match {
            case Some(m) => n = m
            case None => return None
          }
      }
    }
    throw new AssertionError
  }

  /**
   *  Renvoie :
   *  - Some((min, max)) s'il n'y a pas de place, avec la plus petite limite droite et la plus grande limite gauche
   *  - None s'il y a une place
   */
  private def findInColumn(x: Int, id: Int, dx: Int, dy: Int, dom: Domain, tree: RTree[Int, IntRectangle]): Option[(Int, Int)] = {

    var minX = Int.MaxValue
    var maxX = Int.MinValue
    var y: Option[Int] = Some(dom.head)
    while (y.isDefined) {
      // Recherche si la place est libre en (x, y)
      DiffN.treeQueries += 1
      val boxes = tree.search(Rectangle.create(x, y.get, x + dx, y.get + dy)).toBlocking.getIterator
      if (!boxes.hasNext) {

        return None
      } else {
        // Pas de place, on poursuit la recherche au delà du dernier rectangle
        var maxY: Int = Int.MinValue
        do {
          val next = boxes.next
          maxY = math.max(maxY, next.geometry.y2)
          minX = math.min(minX, next.geometry.x2)
          maxX = math.max(maxX, next.geometry.x1)
        } while (boxes.hasNext)

        y = dom.nextOption(maxY - 1)

      }
    }
    Some((minX, maxX))
  }

  private def findInRow(y: Int, id: Int, dx: Int, dy: Int, dom: Domain, tree: RTree[Int, IntRectangle]): Option[(Int, Int)] = {

    var minY = Int.MaxValue
    var maxY = Int.MinValue
    var x: Option[Int] = Some(dom.head)
    while (x.isDefined) {
      DiffN.treeQueries += 1
      val boxes = tree.search(Rectangle.create(x.get, y, x.get + dx, y + dy)).toBlocking.getIterator
      if (!boxes.hasNext) {
        return None
      } else {
        var maxX = Int.MinValue
        do {
          val next = boxes.next
          maxX = math.max(maxX, next.geometry.x2)
          minY = math.min(minY, next.geometry.y2)
          maxY = math.max(maxY, next.geometry.y1)

        } while (boxes.hasNext)

        x = dom.nextOption(maxX - 1)
      }
    }
    Some((minY, maxY))
  }

  def simpleEvaluation: Int = ???

  private def mandatory(ps: ProblemState, i: Int): Option[IntRectangle] = mandatory(ps.dom(xs(i)), ps.dom(ys(i)), ps.dom(dxs(i)).head, ps.dom(dys(i)).head)

  private def mandatory(x: Domain, y: Domain, dx: Int, dy: Int): Option[IntRectangle] = {
    for (xc <- mandatory(x, dx); yc <- mandatory(y, dy)) yield {
      new IntRectangle(xc._1, yc._1, xc._2, yc._2)
    }
  }

  private def mandatory(x: Domain, dx: Int) = {
    val left = x.last
    val right = x.head + dx
    if (left <= right) Some((left, right))
    else None

  }

}

object DiffN extends App {

  @Statistic
  var treeQueries = 0L

  import CSPOM._
  import CSPOMDriver._
  val r = new Random(0)

  val n = 9

  val dxs = IndexedSeq.fill(n)(1 + r.nextInt(5))
  val dys = IndexedSeq.fill(n)(1 + r.nextInt(5))

  val cspom = CSPOM { implicit problem =>

    val maxX = IntVariable(0 until 100) as "maxX"
    val maxY = IntVariable(0 until 100) as "maxY"

    ctr(maxX === maxY)
    val obj = (maxX * maxY) as "obj"

    val xs = Seq.tabulate(n)(i => IntVariable(0 until 100) as s"x$i")
    val ys = Seq.tabulate(n)(i => IntVariable(0 until 100) as s"y$i")

    for (i <- 0 until n) {
      ctr(xs(i) + dxs(i) <= maxX)
      ctr(ys(i) + dys(i) <= maxY)
    }

    ctr(CSPOMConstraint('diffn)(xs, ys, dxs, dys))

    goal(CSPOMGoal.Minimize(maxX))
  }

  val pm = new ParameterManager
  pm("heuristic.value") = classOf[Lexico]

  val solver = Solver(cspom, pm).get

  for (sol <- solver.toIterable) {
    for (i <- 0 until n) println((sol.get(s"x$i").get, sol.get(s"y$i").get, dxs(i), dys(i)))
    println(sol.get("maxX"))
    println("------")
  }

  val stats = new StatisticsManager
  stats.register("solver", solver.solver)
  println(stats)

}