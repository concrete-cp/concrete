package cspfj;

import java.lang.reflect.Field
import scala.xml.NodeSeq
import java.util.Properties
import scala.collection.JavaConversions

/**
 * This class is intended to hold CSP4J's various parameters.
 *
 * @author vion
 *
 */
final object ParameterManager {

  private var parameters: Map[String, (Any, Field)] = Map.empty
  private var pending: Map[String, Any] = Map.empty
  private var pendingParse: Map[String, String] = Map.empty

  def register(o: AnyRef) {
    require(!o.isInstanceOf[Class[_]])
    for (f <- o.getClass.getDeclaredFields) {
      val param = f.getAnnotation(classOf[cspfj.Parameter]);
      if (param != null) {
        val name = param.value
        parameters += name -> (o, f)
        f.setAccessible(true);
        pending.get(name) match {
          case Some(value) => {
            f.set(o, value)
            pending -= name
          }
          case None =>
        }

        pendingParse.get(name) match {
          case Some(s) => {
            f.set(null, parse(f, s))
            pendingParse -= name
          }
          case None =>
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

  private def parse(field: Field, value: String) = {
    var fType = field.getType
    //        if (classOf[Enum[_]].isAssignableFrom(fType)) {
    //          val fClass=field.get(null).getClass.asInstanceOf[Class[Enum[_]]]
    //          
    //            Enum.valueOf(fClass, value)
    //        }

    if (fType.isAssignableFrom(classOf[Int])) {
      value.toInt
    } else if (fType.isAssignableFrom(classOf[Double])) {
      value.toDouble
    } else if (fType.isAssignableFrom(classOf[String])) {
      value.toString
    } else if (fType.isAssignableFrom(classOf[Class[_]])) {
      Class.forName(value)
    } else {
      throw new IllegalArgumentException("Cannot parse " + field + " of type " + fType)
    }
    //    fType match {
    //      //          case e: Class[Enum[_]] => {
    //      //            val fClass = field.get(null).asInstanceOf[Enum[_]]
    //      //          }
    //      case _: Class[Int] => value.toInt
    //      case _: Class[Double] => value.toDouble
    //      case _: Class[String] => value
    //      case _: Class[Class[_]] => Class.forName(value)
    //      case _ => throw new IllegalArgumentException(
    //        "Cannot parse " + field + " of type " + fType)
    //    }

  }

  /**
   * Parses some parameter as a String, and converts it automatically using
   * given registered type.
   *
   * @param name
   * @param value
   */
  def parameterParse(name: String, value: String) {
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
  def toXML = NodeSeq.fromSeq(parameters map {
    case (k, (o, f)) =>
      <p name = { k }>{ f.get(o) }</p>
  } toSeq)

  /**
   * Returns String representation of the registered parameters.
   *
   * @return
   */
  def list = parameters map { p => p._1 + "=" + p._2 } mkString (", ")

  def parseProperties(line: Properties) {
    JavaConversions.mapAsScalaMap(line).foreach {
      case (k, v) => parameterParse(k.toString, v.toString)
    }
  }

}
