package raytracer.resource.yaml.reader

import java.util.{ArrayList => JavaArrayList}
import scala.collection.JavaConverters._

object Converters {
  import ParseResult._

  def conversionError[A](what: String, value: Any): ParseResult[A] =
    fail(s"Failed to convert '$value' to $what")

  def convertPf[A](value: Any, what: String)(pf: PartialFunction[Any, A]): ParseResult[A] = {
    if (pf.isDefinedAt(value)) success(pf(value))
    else conversionError(what, value)
  }

  def toDouble(value: Any): ParseResult[Double] = convertPf(value, "Double") {
    case n: java.lang.Number => n.doubleValue()
    case d: Double => d
  }

  def toBoolean(value: Any): ParseResult[Boolean] = convertPf(value, "Boolean") {
    case b: java.lang.Boolean => b.booleanValue()
    case b: Boolean => b
  }

  def toInt(value: Any): ParseResult[Int] = convertPf(value, "Int") {
    case i: java.lang.Number => i.intValue()
    case i: Int => i
  }

  def toStr(value: Any): ParseResult[String] = convertPf(value, "Double") {
    case s: String => s
  }

  def toMap(value: Any): ParseResult[YamlMap] = convertPf(value, "map") {
    case m: java.util.Map[String, Any] => m.asScala.toMap
    case m: Map[String, Any] => m
  }

  def toVectorAny(value: Any): ParseResult[Vector[Any]] = convertPf(value, "Vector[Any]") {
    case a: JavaArrayList[_] => a.asScala.toVector
    case v: Seq[Any] => v.toVector
  }

  def toVectorDouble(value: Any): ParseResult[Vector[Double]] = {
    toVectorAny(value).flatMap(xs => sequence(xs, toDouble).map(_.toVector))
  }

  def fromTuple3[A](f: (Double, Double, Double) => A)(value: Any): ParseResult[A] =
    toVectorDouble(value) flatMap {
      case Vector(a, b, c) => wrap(f(a, b, c))
      case _ => fail(s"Expected vector[3] of double, found: $value")
    }

  def toPair[A](xs: Seq[A]): ParseResult[(A, A)] = {
    if (xs.size == 2) success((xs.head, xs(1)))
    else fail(s"Expected list of 2, found: $xs")
  }

}
