package cspfj;

import scala.collection.mutable.MultiMap
import scala.collection.mutable.HashMap
import cspfj.util.Loggable
import java.lang.reflect.Modifier
import java.lang.reflect.Field

object StatisticsManager extends Loggable {

  var static: Map[String, Field] = Map.empty

  var objects: Map[String, AnyRef] = Map.empty

  //    private static final Predicate<Field> ANNOTED_FIELD = new Predicate<Field>() {
  //        @Override
  //        public boolean apply(final Field input) {
  //            return input.getAnnotation(cspfj.util.Statistic.class) != null;
  //        }
  //    };
  //    private static final Predicate<Field> STATIC_FIELD = new Predicate<Field>() {
  //        @Override
  //        public boolean apply(final Field input) {
  //            return (input.getModifiers() & Modifier.STATIC) != 0;
  //        }
  //    };
  //    private static final Function<Field, Object> STATIC_FIELD_TO_VALUE = new Function<Field, Object>() {
  //        @Override
  //        public Object apply(final Field value) {
  //            try {
  //                return value.get(null);
  //            } catch (IllegalAccessException e) {
  //                throw new IllegalStateException(e);
  //            }
  //        }
  //    };

//  def register(clazz: Class[_]) {
//    for (f <- clazz.getDeclaredFields if (f.getModifiers & Modifier.STATIC) != 0) {
//      static += (clazz.getName + '.' + f.getName) -> f
//    }
//  }

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
    f.getAnnotation(classOf[cspfj.util.Statistic]) != null &&
      (f.getModifiers & Modifier.STATIC) == 0

  def get(name: String) =
    static.get(name) match {
      case Some(f) => { f.setAccessible(true); f.get(null) }
      case None => {
        val fieldNameAt = name.lastIndexOf('.')
        val obj = objects.get(name.substring(0, fieldNameAt)).get
        val fieldName = name.substring(fieldNameAt + 1, name.length)
        obj.getClass.getDeclaredFields.find(f => annotedInstanceVariable(f) && f.getName == fieldName) match {
          case Some(f) => { f.setAccessible(true); f.get(obj) }
          case None => throw new IllegalArgumentException("Could not find " + name + " (" + fieldName + " in " + obj.getClass.getDeclaredFields.toList + ")")
        }
      }
    }

  def digest =
    (static map { case (k, v) => k -> v.get(null) }) ++
      (objects.foldLeft(Map[String, AnyRef]())((acc, o) => acc ++ o._2.getClass.getDeclaredFields.map { f =>
        (o._1 + "." + f.getName) -> f.get(o._2)
      }))

  def isIntType(input: Class[_]) =
    input == classOf[Int] || input == classOf[Long]

  def isFloatType(input: Class[_]) = input == classOf[Float] || input == classOf[Double]

  def reset() {
    //static = Map.empty
    objects = Map.empty

    for (f <- static.values) {
      f.setAccessible(true)
      if (isIntType(f.getType())) {
        f.set(null, 0);
      } else if (isFloatType(f.getType())) {
        f.set(null, 0f);
      }

    }
  }

}
