package concrete;

import java.util.Properties

import scala.collection.JavaConversions
import scala.collection.mutable.HashMap
import scala.reflect.runtime.universe._

import scala.xml.NodeSeq

/**
 * This class is intended to hold Concrete's various parameters.
 *
 * @author vion
 *
 */
final class ParameterManager {

  private val _parameters: HashMap[String, Any] = new HashMap()

  val used = collection.mutable.Set[String]()

  /**
   * Updates some parameter, overriding default or previous value.
   *
   * @param name
   * @param value
   */
  def update(name: String, value: Any) {
    _parameters(name) = value
  }

  def getOrElse[T: TypeTag](name: String, default: => T): T = {
    get[T](name).getOrElse(default)
  }

  def get[T: TypeTag](name: String): Option[T] = {
    require(TypeTag.Nothing != typeTag[T], s"Please give a type for $name, was ${typeTag[T]}")
    val got = parameters.get(name).map {
      case s: String => parse(typeOf[T], s)
      case v: Any    => v.asInstanceOf[T]
    }
    if (got.isDefined) used += name
    got
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
      value.toString
    } else if (fType <:< typeOf[Class[_]]) {
      Class.forName(value)
    } else {
      throw new IllegalArgumentException(s"Cannot parse $value of type $fType")
    }).asInstanceOf[T]

  }

  /**
   * Returns XML representation of the registered parameters.
   *
   * @return
   */
  def toXML = NodeSeq.fromSeq(parameters.iterator.map {
    case (k, v) =>
      <p name={ k }>{ v }</p>
  }.toSeq)

  /**
   * Returns String representation of the registered parameters.
   *
   * @return
   */
  override def toString = _parameters.iterator.map { case (k, v) => k + "=" + v }.mkString(", ")

  def parameters: collection.immutable.Map[String, Any] = _parameters.toMap
  //  ++ pending.iterator.map {
  //      case (k, v) => k -> v
  //    } ++ pendingParse.iterator.map {
  //      case (k, v) => k -> v
  //    } toMap

  def parseProperties(line: Properties) {
    JavaConversions.mapAsScalaMap(line).foreach {
      case (k, v) => update(k.toString, v.toString)
    }
  }

}
