package concrete
package constraint
package semantic

import bitvectors.BitVector
import com.github.davidmoten.rtree.RTree
import com.github.davidmoten.rtree.geometry.{Geometries, Geometry, Rectangle}
import concrete.util.Interval
import cspom._

class IntRectangle(
                    val x1: Int,
                    val y1: Int,
                    val x2: Int,
                    val y2: Int) extends Geometry {

  assert(x2 >= x1)

  assert(y2 >= y1)

  lazy val asRectangle: Rectangle = Geometries.rectangle(x1, y1, x2, y2)

  override def intersects(r: com.github.davidmoten.rtree.geometry.Rectangle): Boolean = {
    r.x2 > x1 && r.x1 < x2 && r.y2 > y1 && r.y1 < y2
  }

  override def distance(r: Rectangle): Double =
    asRectangle.distance(r)

  override def toString: String = {
    s"Rectangle [x1=$x1, y1=$y1, x2=$x2, y2=$y2]"
  }

  override def hashCode(): Int = (x1, y1, x2, y2).hashCode

  override def equals(obj: Any): Boolean = {
    obj match {
      case other: IntRectangle => x1 == other.x1 && x2 == other.x2 && y1 == other.y1 && y2 == other.y2
      case _ => false
    }
  }

  def mbr: Rectangle = asRectangle

  override def isDoublePrecision: Boolean = false
}

case class RectangleBounds(minDx: Int, minDy: Int, minX: Int, maxX: Int, minY: Int, maxY: Int) extends Ordered[RectangleBounds] {
  val coef: Double = minSurface.toDouble / availableSurface

  def minSurface: Int = minDx * minDy

  def xSpan = Interval(minX, maxX)

  def ySpan = Interval(minY, maxY)

  def availableSurface: Int = (maxX - minX) * (maxY - minY)

  def compare(t: RectangleBounds): Int = java.lang.Double.compare(coef, t.coef)
}

object DiffN {
  @Statistic
  var treeQueries: Long = 0L
}

trait DiffNChecker {

  def nbRectangles: Int

  def check(tuple: Array[Int]): Boolean = {
    val Seq(x, y, dx, dy) = tuple.grouped(nbRectangles).toSeq

    Iterator.range(0, nbRectangles).forall { i =>
      Iterator.range(0, i).forall { j =>
        x(i) + dx(i) <= x(j) || y(i) + dy(i) <= y(j) ||
          x(j) + dx(j) <= x(i) || y(j) + dy(j) <= y(i)
      }
    }
  }

}

class DiffNSpaceChecker(xs: Array[Variable], ys: Array[Variable], dxs: Array[Variable], dys: Array[Variable])
  extends Constraint(xs ++ ys ++ dxs ++ dys)
    with DiffNChecker {

  assert(ys.length == nbRectangles && dxs.length == nbRectangles && dys.length == nbRectangles)

  def advise(ps: ProblemState, event: Event, pos: Int): Int = if (event <= BoundRemoval) nbRectangles else -1

  def init(ps: ProblemState): Outcome = ps

  def revise(ps: ProblemState, mod: BitVector): Outcome = {
    val rectangles = Array.tabulate(nbRectangles) { i =>
      val domX = ps.dom(xs(i))
      val domY = ps.dom(ys(i))
      val dx = ps.dom(dxs(i))
      val dy = ps.dom(dys(i))
      RectangleBounds(dx.head, dy.head, domX.head, domX.last + dx.last, domY.head, domY.last + dy.last)
    }

    scala.util.Sorting.quickSort(rectangles)

    var xSpan = rectangles(0).xSpan
    var ySpan = rectangles(0).ySpan

    var minArea = rectangles(0).minSurface

    for (i <- 1 until xs.length) {
      val r = rectangles(i)

      xSpan = xSpan span r.xSpan
      ySpan = ySpan span r.ySpan
      minArea += r.minSurface

      // Do not use interval size as it counts discrete elements, not
      // interval length
      val availableArea = (xSpan.ub - xSpan.lb) * (ySpan.ub - ySpan.lb)

      if (availableArea < minArea) {
        return Contradiction(scope)
      }
    }

    ps

  }

  def nbRectangles: Int = xs.length

  def simpleEvaluation: Int = 2

}

class DiffN(xs: Array[Variable], ys: Array[Variable], dxs: Array[Variable], dys: Array[Variable]) extends Constraint(xs ++ ys ++ dxs ++ dys)
  with StatefulConstraint[(RTree[Int, IntRectangle], Vector[Option[(Int, IntRectangle)]])] with BC with DiffNChecker with FixPoint {
  assert(ys.length == nbRectangles && dxs.length == nbRectangles && dys.length == nbRectangles)

  def advise(problemState: ProblemState, pos: Int): Int = nbRectangles

  def init(ps: ProblemState): concrete.Outcome = {
    val map: Vector[Option[(Int, IntRectangle)]] = Vector.tabulate(xs.length)(i => mandatory(ps, i).map((i, _)))
    val tree = map.foldLeft(RTree.create[Int, IntRectangle]) { (t, or) =>
      or.foldLeft(t) { case (t, (i, r)) =>
        if (!t.search(r.asRectangle).isEmpty.toBlocking.single) {
          return Contradiction(scope)
        }
        t.add(i, r)
      }
    }

    ps.updateState(this, (tree, map))

  }

  def shave(ps: ProblemState): Outcome = ???

  override def revise(ps: ProblemState, mod: BitVector): Outcome = {
    var (tree, map) = ps(this)

    {
      // Update RTree
      val xyRect = mod | mod.shift(-2 * nbRectangles)
      val modRectangles = xyRect | xyRect.shift(-nbRectangles)
      //println(s"modified $mod, so rectangles $modRectangles")

      var i = modRectangles.nextSetBit(0)

      while (i >= 0 && i < xs.length) {
        for (
          nr <- mandatory(ps, i) if !map(i).contains((i, nr)) // TODO: late bug correction
        ) {
          for (r <- map(i)) tree = tree.delete(r._1, r._2)

          if (!tree.search(nr.asRectangle).isEmpty.toBlocking.single) {
            return Contradiction(scope)
          }

          map = map.updated(i, Some((i, nr)))
          tree = tree.add(i, nr)

        }
        i = modRectangles.nextSetBit(i + 1)
      }

    }

    // Filter bounds until fixpoint

    fixPoint(ps, xs.indices, { (state: ProblemState, j: Int) =>

      val treeWoutMe = map(j).foldLeft(tree)((t, e) => t.delete(e._1, e._2))

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
        case None => Contradiction(scope)
        case Some((newX, newY)) =>

          val ns = state
            .shaveDomNonEmpty(xs(j), newX)
            .shaveDomNonEmpty(ys(j), newY)
          if (ns ne state) {

            mandatory(ns, j) match {
              case Some(nr) =>
                DiffN.treeQueries += 1
                if (!treeWoutMe.search(nr.asRectangle).isEmpty.toBlocking.single) {
                  return Contradiction(scope)
                }

                tree = treeWoutMe.add(j, nr)
                map = map.updated(j, Some((j, nr)))

              case None =>
                require(map(j).isEmpty)
              //map = map.updated(i, None)
            }

          }
          ns
      }

    })
      .updateState(this, (tree, map))

  }

  def nbRectangles: Int = xs.length

  private def mandatory(ps: ProblemState, i: Int): Option[IntRectangle] =
    mandatory(ps.dom(xs(i)), ps.dom(ys(i)), ps.dom(dxs(i)).head, ps.dom(dys(i)).head)

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

  //  val xResidue = new HashMap[(Int, Int), Int]
  //
  //  private def checkXResidue(x: Int, id: Int, dx: Int, dy: Int, dom: Domain, tree: RTree[Int, IntRectangle]): Boolean = {
  //    xResidue.get((x, id)) match {
  //      case Some(y) => dom.present(y) && tree.search(Rectangle.create(x, y, x + dx, y + dy)).isEmpty.toBlocking.single
  //      case None => false
  //    }
  //  }
  //
  //  val yResidue = new HashMap[(Int, Int), Int]
  //
  //  private def checkYResidue(y: Int, id: Int, dx: Int, dy: Int, dom: Domain, tree: RTree[Int, IntRectangle]): Boolean = {
  //    xResidue.get((y, id)) match {
  //      case Some(x) => dom.present(x) && tree.search(Rectangle.create(x, y, x + dx, y + dy)).isEmpty.toBlocking.single
  //      case None => false
  //    }
  //  }

  /**
    * Renvoie :
    *  - Some((min, max)) s'il n'y a pas de place, avec la plus petite limite droite et la plus grande limite gauche
    *  - None s'il y a une place
    */
  private def findInColumn(x: Int, id: Int, dx: Int, dy: Int, dom: Domain, tree: RTree[Int, IntRectangle]): Option[(Int, Int)] = {

    //if (checkXResidue(x, id, dx, dy, dom, tree)) return None

    var minX = Int.MaxValue
    var maxX = Int.MinValue
    var y: Option[Int] = Some(dom.head)
    while (y.isDefined) {
      // Recherche si la place est libre en (x, y)
      DiffN.treeQueries += 1
      val boxes = tree.search(Geometries.rectangle(x, y.get, x + dx, y.get + dy)).toBlocking.getIterator
      if (!boxes.hasNext) {
        //xResidue((x, id)) = y.get
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

  private def findInRow(y: Int, id: Int, dx: Int, dy: Int, dom: Domain, tree: RTree[Int, IntRectangle]): Option[(Int, Int)] = {

    // if (checkYResidue(y, id, dx, dy, dom, tree)) return None

    var minY = Int.MaxValue
    var maxY = Int.MinValue
    var x: Option[Int] = Some(dom.head)
    while (x.isDefined) {
      DiffN.treeQueries += 1
      val boxes = tree.search(Geometries.rectangle(x.get, y, x.get + dx, y + dy)).toBlocking.getIterator
      if (!boxes.hasNext) {
        //yResidue((y, id)) = x.get
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

}
