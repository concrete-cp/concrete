package concrete



import java.util.Properties

import com.typesafe.scalalogging.LazyLogging

import scala.jdk.CollectionConverters._
import scala.collection.immutable.{SortedMap, TreeMap}
import scala.collection.mutable
import scala.reflect.runtime.universe._


/**
  * This class is intended to hold Concrete's various parameters.
  *
  * @author vion
  *
  */
final class ParameterManager(
                              val parameters: SortedMap[String, Any],
                              used: mutable.HashSet[String]) extends LazyLogging {

  def this() = this(new TreeMap(), new mutable.HashSet[String]())

  def +(name: String): ParameterManager = updated(name, ())

  /**
    * Updates some parameter, overriding default or previous value.
    *
    * @param name
    * @param value
    */
  def updated(name: String, value: Any): ParameterManager = {
    new ParameterManager(
      parameters + (name -> value),
      used)
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
    for (g <- got) {
      logger.info(s"$name = $g")
      used += name
    }
    got
  }

  private def parse[T](fType: Type, value: String): T = {
    (if (fType <:< typeOf[Int]) {
      value.toInt
    } else if (fType <:< typeOf[Long]) {
      value.toLong
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

  def classInPackage[T](name: String, pack: String, default: => Class[_ <: T]): Class[T] = {
    (parameters.get(name) match {
      case Some(s: String) =>
        used += name
        ParameterManager.classInPackage(s, pack)
      case Some(c) =>
        used += name
        c
      case None => default
    })
      .asInstanceOf[Class[T]]

  }


  def contains(name: String): Boolean = {
    val c = parameters.contains(name)
    if (c) used += name
    c
  }

  def unused: Set[String] = parameters.keySet -- used

  /**
    * Returns String representation of the registered parameters.
    *
    * @return
    */
  override def toString: String = parameters.iterator.map {
    case (k, ()) => k
    case (k, v) => s"$k = $v"
  }.mkString(", ")

  def parseProperties(line: Properties): ParameterManager = {
    line.asScala.foldLeft(this) {
      case (acc, (k, v)) => acc.updated(k.toString, v.toString)
    }
  }

}

object ParameterManager {
  def classInPackage(name: String, pack: String): Class[_] = {
    try Class.forName(name)
    catch {
      case _: ClassNotFoundException => Class.forName(s"$pack.$name")
    }
  }
}