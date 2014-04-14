package concrete;

import java.lang.reflect.Field
import scala.xml.NodeSeq
import java.util.Properties
import scala.collection.JavaConversions
import java.security.InvalidParameterException
import scala.collection.mutable.HashMap
import scala.annotation.meta.field
import scala.reflect.ClassTag
import scala.annotation.meta.field
import scala.reflect.runtime.universe._

/**
 * This class is intended to hold Concrete's various parameters.
 *
 * @author vion
 *
 */
final class ParameterManager {

  private val parameters: HashMap[String, Any] = new HashMap()

  /**
   * Updates some parameter, overriding default or previous value.
   *
   * @param name
   * @param value
   */
  def update(name: String, value: Any) {
    parameters(name) = value
  }

  def apply[T: TypeTag](name: String): Option[T] = parameters.get(name).map {
    case s: String => parse(typeOf[T], s)
    case v: Any =>
      require(typeOf[T] == typeOf[Any])
      v.asInstanceOf[T]
  }

  private def parse[T](fType: Type, value: String): T = {
    (if (fType <:< typeOf[Int]) {
      value.toInt
      //    } else if (fType.isAssignableFrom(classOf[Boolean])) {
      //      value.toBoolean
      //    } else if (fType.isAssignableFrom(classOf[Double])) {
      //      value.toDouble
      //    } else if (fType.isAssignableFrom(classOf[String])) {
      //      value.toString
      //    } else if (fType.isAssignableFrom(classOf[Class[_]])) {
      //      Class.forName(value)
    } else {
      throw new IllegalArgumentException(s"Cannot parse $value of type $fType")
    }).asInstanceOf[T]

  }

  /**
   * Parses some parameter as a String, and converts it automatically using
   * given registered type.
   *
   * @param name
   * @param value
   */
  def parse(name: String, value: String) {
    parameters(name) = value
//    parameters.get(name) match {
//      case None => pendingParse += name -> value
//      case Some((o, f)) => f.set(o, parse(f, value))
//    }
  }

  /**
   * Returns XML representation of the registered parameters.
   *
   * @return
   */
  def toXML = NodeSeq.fromSeq(allParams.iterator.map {
    case (k, v) =>
      <p name={ k }>{ v }</p>
  }.toSeq)

  /**
   * Returns String representation of the registered parameters.
   *
   * @return
   */
  def list = allParams.iterator.map { case (k, v) => k + "=" + v }.mkString(", ")

  def allParams: Map[String, Any] = parameters.toMap
//  ++ pending.iterator.map {
//      case (k, v) => k -> v
//    } ++ pendingParse.iterator.map {
//      case (k, v) => k -> v
//    } toMap

  def parseProperties(line: Properties) {
    JavaConversions.mapAsScalaMap(line).foreach {
      case (k, v) => parse(k.toString, v.toString)
    }
  }

}
