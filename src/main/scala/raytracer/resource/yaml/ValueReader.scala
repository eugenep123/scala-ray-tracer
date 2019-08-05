package raytracer.resource.yaml

// Used to read.convert a java Yaml (Any) value to some type A
trait ValueReader[A] {
  def read(value: Any): ParseResult[A]

  final def flatMap[B](f: A => ParseResult[B]): ValueReader[B] = {
    (value: Any) => ValueReader.this.read(value).flatMap(f)
  }
  final def map[B](f: A => B): ValueReader[B] = {
    (value: Any) => ValueReader.this.read(value).map(f)
  }
}

object ValueReader {
  def apply[A](implicit ev: ValueReader[A]): ValueReader[A] = ev
}

