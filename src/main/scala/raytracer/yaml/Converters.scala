package raytracer.yaml

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

  implicit class YamlMapOps(val map: YamlMap) extends AnyVal {

    def readWith[A](key: String, convert: Any => ParseResult[A]): ParseResult[A] = {
      val result = for {
        any <- wrap(map(key)).withMessage(s"Failed to read key: '$key'")
        value <- convert(any)
      } yield value
      result.withDetail(map)
    }

    def readOptWith[A](key: String, convert: Any => ParseResult[A]): ParseResult[Option[A]] = {
      map.get(key) match {
        case None => success(None)
        case Some(value) =>
          convert(value).withDetail(map).map(Option.apply)
      }
    }

    def read[A](key: String)(implicit reader: ValueReader[A]): ParseResult[A] =
      readWith(key, reader.read)

    def readOpt[A](key: String)(implicit reader: ValueReader[A]): ParseResult[Option[A]] =
      readOptWith(key, reader.read)

    def readDouble(key: String): ParseResult[Double] = readWith(key, toDouble)
    def readBool(key: String): ParseResult[Boolean] = readWith(key, toBoolean)
    def readDoubleOpt(key: String): ParseResult[Option[Double]] = readOptWith(key, toDouble)
    def readInt(key: String): ParseResult[Int] = readWith(key, toInt)
    def readString(key: String): ParseResult[String] = readWith(key, toStr)
    def convertTo[A](implicit reader: ValueReader[A]): ParseResult[A] = {
      reader.read(map).withDetail(map)
    }
  }
}
