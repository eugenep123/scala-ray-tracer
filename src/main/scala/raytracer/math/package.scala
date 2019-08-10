package raytracer

package object math {
  // Use 2 epsilons, one for under/over Point3D and another for testing
  val EPSILON      = 0.000001d //0.0001d //original: 0.0001d
  val EPSILON_TEST = 0.0001d

  val INFINITY = 1e10 //1x10 pow 10
  val π = Math.PI
  val PI = Math.PI
  type Radians = Double

  @inline def doubleEquals(a: Double, b: Double): Boolean =
    Math.abs(a - b) < EPSILON_TEST

  @inline def doubleEquals(tup: (Double, Double)): Boolean =
    doubleEquals(tup._1, tup._2)

  def doubleToString(d: Double): String = {
    val s = d.toString
    val s2 =
      if (s.endsWith(".0")) s.stripSuffix(".0")
      else s
    if (s2 == "-0") "0" else s2
  }

  @inline def requireInRange(value: Int, min: Int, max: Int, what: String): Unit =
    require(value >= min && value <= max, s"$what is out of range ($min - $max): $value")

  implicit class DoubleOps(val a: Double) extends AnyVal {
    @inline def ~=(b: Double): Boolean = doubleEquals(a, b)
  }

  implicit class StringOps(val s: String) extends AnyVal {
    def matrix: Matrix = Matrix.parse(s)
  }

  implicit def transformationsToMatrix(t: TransformBuilder): Matrix = t.build()
  implicit def operationToMatrix(o: Transform): Matrix = o.matrix

  def mkString(name: String, xs: Double*): String = {
    xs.map(doubleToString).mkString(s"$name(", ", ", ")")
  }

  @inline def max(a: Double, b: Double, c: Double): Double =
    scala.math.max(a, scala.math.max(b, c))

  @inline def min(a: Double, b: Double, c: Double): Double =
    scala.math.min(a, scala.math.min(b, c))


  implicit class Tap[A](val underlying: A) extends AnyVal {
    def tap(func: A => Unit): A = {
      func(underlying)
      underlying
    }
  }
}
