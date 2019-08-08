package raytracer.resource.yaml.reader

import raytracer.Color
import raytracer.math.{Point3D, Vector3D}

object Readers {
  import ParseResult._
  import raytracer.resource.yaml.reader.Converters._

  implicit val ColorReader: ValueReader[Color] = (value: Any) => fromTuple3(Color.apply)(value)
  implicit val Point3DReader: ValueReader[Point3D] = (value: Any) => fromTuple3(Point3D.apply)(value)
  implicit val Vector3DReader: ValueReader[Vector3D] = (value: Any) => fromTuple3(Vector3D.apply)(value)

  trait MapReader[A] extends ValueReader[A] {
    override def read(value: Any): ParseResult[A] =
      Converters.toMap(value).flatMap(readMap)
    def readMap(map: YamlMap): ParseResult[A]
  }

  trait ListReader[A] extends ValueReader[A] {
    override def read(value: Any): ParseResult[A] =
      Converters.toVectorAny(value).flatMap(readSeq)
    def readSeq(xs: Vector[Any]): ParseResult[A]
  }

  implicit def listReader[A](implicit itemReader: ValueReader[A]) = new ValueReader[Seq[A]] {
    override def read(value: Any): ParseResult[Seq[A]] = {
      Converters.toVectorAny(value).flatMap(readSeq)
    }
    def readSeq(xs: Vector[Any]): ParseResult[Seq[A]] = {
      ParseResult.sequence(xs, itemReader.read)
    }
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
    def readDoubleOpt(key: String): ParseResult[Option[Double]] = readOptWith(key, toDouble)
    def readBool(key: String): ParseResult[Boolean] = readWith(key, toBoolean)
    def readBoolOpt(key: String): ParseResult[Option[Boolean]] = readOptWith(key, toBoolean)
    def readInt(key: String): ParseResult[Int] = readWith(key, toInt)
    def readString(key: String): ParseResult[String] = readWith(key, toStr)
    def readStringOpt(key: String): ParseResult[Option[String]] = readOptWith(key, toStr)
    def convertTo[A](implicit reader: ValueReader[A]): ParseResult[A] = {
      reader.read(map).withDetail(map)
    }
    def readMap(key: String): ParseResult[YamlMap] = readWith(key, toMap)
  }




}
