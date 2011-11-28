package cspfj;

import scala.collection.mutable.MultiMap
import scala.collection.mutable.HashMap
import cspfj.util.Loggable
import java.lang.reflect.Modifier
import java.lang.reflect.Field
import scala.annotation.tailrec

object StatisticsManager extends Loggable {

  var objects: Map[String, AnyRef] = Map.empty

  def register(name: String, o: AnyRef) {
    require(!o.isInstanceOf[Class[_]])
    if (objects.contains(name)) {
      info(name + ": an object with the same name is already registered");
    }

    objects += name -> o
    //    for (
    //      f <- o.getClass.getDeclaredFields if annotedInstanceVariable(f)
    //    ) {
    //      f.setAccessible(true);
    //    }
  }

  private def annotedInstanceVariable(f: Field) =
    f.getAnnotation(classOf[cspfj.Statistic]) != null &&
      (f.getModifiers & Modifier.STATIC) == 0

  def get(name: String) = {

    val fieldNameAt = name.lastIndexOf('.')
    val obj = objects.get(name.substring(0, fieldNameAt)).get
    val fieldName = name.substring(fieldNameAt + 1, name.length)
    obj.getClass.getDeclaredFields.find(f => annotedInstanceVariable(f) && f.getName == fieldName) match {
      case Some(f) => { f.setAccessible(true); f.get(obj) }
      case None => throw new IllegalArgumentException("Could not find " + name + " (" + fieldName + " in " + obj.getClass.getDeclaredFields.toList + ")")
    }

  }

  def digest = (objects map {
    case (s, o) =>
      o.getClass.getDeclaredFields.map { f =>
        (s + "." + f.getName) -> f.get(o)
      }
  } flatten).toMap

  def isIntType(input: Class[_]) =
    input == classOf[Int] || input == classOf[Long]

  def isFloatType(input: Class[_]) = input == classOf[Float] || input == classOf[Double]

  def reset() {
    //static = Map.empty
    objects = Map.empty

  }

  def average[A](s: Seq[A])(implicit n: Numeric[A]) = n.toDouble(s.sum) / s.size

  def stDev[A](s: Seq[A])(implicit n: Numeric[A]) = {
    val avg = average(s)
    val sumSq = s map (v => math.pow(n.toDouble(v) - avg, 2)) sum

    math.sqrt(sumSq / (s.size - 1))
  }

  @tailrec
  def findKMedian[A](arr: Seq[A], k: Int, o: Ordering[A]): A = {
    val pivot = arr(scala.util.Random.nextInt(arr.size))
    val (s, b) = arr partition (o.gt(pivot, _))
    if (s.size == k) pivot
    // The following test is used to avoid infinite repetition
    else if (s.isEmpty) {
      val (s, b) = arr partition (pivot ==)
      if (s.size > k) pivot
      else findKMedian(b, k - s.size, o)
    } else if (s.size < k) findKMedian(b, k - s.size, o)
    else findKMedian(s, k, o)
  }

  def median[A](arr: Seq[A])(implicit o: Ordering[A]) =
    findKMedian(arr, arr.size / 2, o)

  def time(f: => Unit) = {
    var t = -System.currentTimeMillis
    f
    t += System.currentTimeMillis()
    t / 1000f
  }

}
