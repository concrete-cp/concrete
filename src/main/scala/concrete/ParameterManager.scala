package concrete;

import java.lang.reflect.Field
import scala.xml.NodeSeq
import java.util.Properties
import scala.collection.JavaConversions
import java.security.InvalidParameterException
import scala.collection.mutable.HashMap

/**
 * This class is intended to hold CSP4J's various parameters.
 *
 * @author vion
 *
 */
final object ParameterManager {

  private val parameters: HashMap[String, (Any, Field)] = new HashMap()
  private val pending: HashMap[String, Any] = new HashMap()
  private val pendingParse: HashMap[String, String] = new HashMap()

  def register(o: AnyRef) {
    require(!o.isInstanceOf[Class[_]])
    for (f <- o.getClass.getDeclaredFields) {
      val param = f.getAnnotation(classOf[concrete.Parameter]);
      if (param != null) {
        val name = param.value
        parameters += name -> (o, f)
        f.setAccessible(true);
        for (value <- pending.get(name)) {
          f.set(o, value)
          pending -= name
        }
        for (s <- pendingParse.get(name)) {
          f.set(o, parse(f, s))
          pendingParse -= name
        }
      }
    }
  }

  /**
   * Updates some parameter, overriding default or previous value.
   *
   * @param name
   * @param value
   */
  def update(name: String, value: Any) {
    parameters.get(name) match {
      case None => pending += name -> value
      case Some((o, f)) => f.set(o, value)
    }
  }

  private def parse(field: Field, value: String): Any = {
    val fType = field.getType

    if (fType.isAssignableFrom(classOf[Int])) {
      value.toInt
    } else if (fType.isAssignableFrom(classOf[Boolean])) {
      value.toBoolean
    } else if (fType.isAssignableFrom(classOf[Double])) {
      value.toDouble
    } else if (fType.isAssignableFrom(classOf[String])) {
      value.toString
    } else if (fType.isAssignableFrom(classOf[Class[_]])) {
      Class.forName(value)
    } else {
      throw new IllegalArgumentException(s"Cannot parse $field of type $fType")
    }

  }

  /**
   * Parses some parameter as a String, and converts it automatically using
   * given registered type.
   *
   * @param name
   * @param value
   */
  def parse(name: String, value: String) {
    parameters.get(name) match {
      case None => pendingParse += name -> value
      case Some((o, f)) => f.set(o, parse(f, value))
    }
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

  def allParams: Map[String, Any] =
    parameters.iterator.map {
      case (k, (o, f)) => k -> f.get(o)
    } ++ pending.iterator.map {
      case (k, v) => k -> v
    } ++ pendingParse.iterator.map {
      case (k, v) => k -> v
    } toMap

  def parseProperties(line: Properties) {
    JavaConversions.mapAsScalaMap(line).foreach {
      case (k, v) => parse(k.toString, v.toString)
    }
  }

  def checkPending() {
    if (pending.nonEmpty) throw new InvalidParameterException("These parameters were not used " + pending)
    if (pendingParse.nonEmpty) throw new InvalidParameterException("These parameters were not used " + pendingParse)

  }

}
