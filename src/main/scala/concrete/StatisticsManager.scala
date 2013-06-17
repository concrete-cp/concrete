package concrete;

import scala.collection.mutable.MultiMap
import scala.collection.mutable.HashMap
import concrete.util.Loggable
import java.lang.reflect.Modifier
import java.lang.reflect.Field
import scala.annotation.tailrec

class StatisticsManager extends Loggable {

  var objects: Map[String, AnyRef] = Map.empty

  def register(name: String, o: AnyRef) {
    require(!o.isInstanceOf[Class[_]])
    if (objects.contains(name)) {
      logger.info(name + ": an object with the same name is already registered");
    }

    objects += name -> o
  }

  private def annoted(f: Field) = f.getAnnotation(classOf[concrete.Statistic]) != null

  def apply(name: String): AnyRef = {

    val fieldNameAt = name.lastIndexOf('.')
    val obj = objects.get(name.substring(0, fieldNameAt)).get
    val fieldName = name.substring(fieldNameAt + 1, name.length)
    fields(obj.getClass).find(f => f.getName == fieldName) match {
      case Some(f) => { f.setAccessible(true); f.get(obj) }
      case None => throw new IllegalArgumentException(
        s"Could not find $name ($fieldName in ${fields(obj.getClass)})")
    }

  }

  def digest: Map[String, Any] = digest("")

  private def fields(c: Class[_], f: List[Field] = Nil): List[Field] =
    if (c == null) {
      f
    } else {
      fields(c.getSuperclass, c.getDeclaredFields.toList.filter(annoted) ::: f)
    }

  private def digest(sub: String): Map[String, Any] = objects flatMap {
    case (s, o) =>
      fields(o.getClass).flatMap { f =>
        f.setAccessible(true)
        f.get(o) match {
          case sm: StatisticsManager => sm.digest("%s.%s.".format(s, f.getName))
          case v: AnyRef => Map(sub + s + "." + f.getName -> v)
        }
      }
  }

  override def toString = digest.map(t => t._1 + " = " + t._2).toSeq.sorted.mkString("\n")

  private def isIntType(input: Class[_]) = input == classOf[Int] || input == classOf[Long]

  private def isFloatType(input: Class[_]) = input == classOf[Float] || input == classOf[Double]

  def reset() {
    objects = Map.empty
  }
}

object StatisticsManager {

  def average[A](s: Seq[A])(implicit n: Numeric[A]): Double = average(s.iterator)
  def average[A](s: Iterator[A])(implicit n: Numeric[A]): Double = {
    val (sum, count) = s.foldLeft((n.zero, 0L)) {
      case ((cs, cc), i) =>
        (n.plus(cs, i), cc + 1L)
    }

    n.toDouble(sum) / count
  }

  def variance[A](s: Seq[A])(implicit n: Numeric[A]): Double = {
    val avg = average(s)
    val sumSq = s map (v => math.pow(n.toDouble(v) - avg, 2)) sum

    sumSq / (s.size - 1)
  }

  def stDev[A](s: Seq[A])(implicit n: Numeric[A]): Double = math.sqrt(variance(s))

  @tailrec
  def findKMedian[A](arr: Seq[A], k: Int)(implicit o: Ordering[A]): A = {
    val pivot = arr(scala.util.Random.nextInt(arr.size))
    val (s, b) = arr partition (o.gt(pivot, _))
    if (s.size == k) {
      pivot
    } // The following test is used to avoid infinite repetition
    else if (s.isEmpty) {
      val (s, b) = arr partition (pivot ==)
      if (s.size > k) {
        pivot
      } else {
        findKMedian(b, k - s.size)
      }
    } else if (s.size < k) {
      findKMedian(b, k - s.size)
    } else {
      findKMedian(s, k)
    }
  }

  def median[A](arr: Seq[A])(implicit o: Ordering[A]): A = {
    if (arr.isEmpty) {
      throw new NoSuchElementException("Median of empty sequence")
    } else {
      findKMedian(arr, arr.size / 2)
    }
  }

  def fq[A](arr: Seq[A])(implicit o: Ordering[A]): A = {
    if (arr.isEmpty) {
      throw new NoSuchElementException("Median of empty sequence")
    } else {
      findKMedian(arr, arr.size / 4)
    }
  }
  def tq[A](arr: Seq[A])(implicit o: Ordering[A]): A = {
    if (arr.isEmpty) {
      throw new NoSuchElementException("Median of empty sequence")
    } else {
      findKMedian(arr, 3 * arr.size / 4)
    }
  }
  def time[A](f: => A): (A, Double) = {
    var t = -System.currentTimeMillis
    try {
      val r = f
      t += System.currentTimeMillis()
      (r, t / 1000.0)
    } catch {
      case e: Throwable =>
        t += System.currentTimeMillis()
        throw new TimedException(t / 1000.0, e)
    }
  }

}

class TimedException(val time: Double, cause: Throwable) extends Exception(cause)
