package concrete;

import java.util.Properties
import scala.collection.JavaConversions
import scala.reflect.runtime.universe._
import scala.collection.mutable.HashSet
import scala.collection.immutable.TreeMap
import scala.collection.immutable.SortedMap
import com.typesafe.scalalogging.LazyLogging

/**
 * This class is intended to hold Concrete's various parameters.
 *
 * @author vion
 *
 */
final class ParameterManager extends LazyLogging {

  private var _parameters: SortedMap[String, Any] = new TreeMap()

  val used = new HashSet[String]()

  /**
   * Updates some parameter, overriding default or previous value.
   *
   * @param name
   * @param value
   */
  def update(name: String, value: Any) {
    _parameters += (name -> value)
  }

  def getOrElse[T: TypeTag](name: String, default: => T): T = {
    get[T](name).getOrElse(default)
  }

  def get[T: TypeTag](name: String): Option[T] = {
    require(TypeTag.Nothing != typeTag[T], s"Please give a type for $name, was ${typeTag[T]}")
    getRaw(name).map {
      case s: String => parse(typeOf[T], s)
      case v: Any => v.asInstanceOf[T]
    }
  }

  def getRaw(name: String): Option[Any] = {
    val got = parameters.get(name)
    if (got.isDefined) {
      logger.info(s"$name = $got")
      used += name
    }
    got
  }

  def classInPackage[T](name: String, pack: String, default: => Class[_ <: T]): Class[T] = {
    (parameters.get(name) match {
      case Some(s: String) =>
        used += name
        classInPackage(s, pack)
      case Some(c) =>
        used += name
        c
      case None => default
    })
      .asInstanceOf[Class[T]]

  }

  def classInPackage(name: String, pack: String): Class[_] = {
    try Class.forName(name)
    catch {
      case _: ClassNotFoundException => Class.forName(s"$pack.$name")
    }
  }

  def contains(name: String) = {
    val c = parameters.contains(name)
    if (c) used += name
    c
  }

  def unused = parameters.keySet -- used

  private def parse[T](fType: Type, value: String): T = {
    (if (fType <:< typeOf[Int]) {
      value.toInt
    } else if (fType <:< typeOf[Boolean]) {
      value.toBoolean
    } else if (fType <:< typeOf[Double]) {
      value.toDouble
    } else if (fType <:< typeOf[String]) {
      value
    } else if (fType <:< typeOf[Class[_]]) {
      Class.forName(value)
    } else {
      throw new IllegalArgumentException(s"Cannot parse $value of type $fType")
    }).asInstanceOf[T]

  }

  /**
   * Returns String representation of the registered parameters.
   *
   * @return
   */
  override def toString = _parameters.iterator.map {
    case (k, Unit) => k
    case (k, v) => s"$k = $v"
  }.mkString(", ")

  def parameters: SortedMap[String, Any] = _parameters

  def parseProperties(line: Properties) {
    JavaConversions.mapAsScalaMap(line).foreach {
      case (k, v) => update(k.toString, v.toString)
    }
  }

}
