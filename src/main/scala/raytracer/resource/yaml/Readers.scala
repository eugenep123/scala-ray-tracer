package raytracer.resource.yaml

object Readers {

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
    def readSeq(xs: Vector[Any]) = {
      ParseResult.sequence(xs, itemReader.read)
    }
  }


}
