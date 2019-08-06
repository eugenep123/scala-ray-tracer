package raytracer.math

import scala.math.abs

final case class Tuple4(
  x: Double,
  y: Double,
  z: Double,
  w: Double) {

  def isPoint: Boolean = w == 1.0
  def isVector: Boolean = w == 0.0

  override def toString: String = mkString("tuple", x, y, z, w)

  override def equals(obj: Any): Boolean = {
    obj match {
      case rhs: Tuple4 => tupleEquals(rhs)
      case rhs: Vector3D => tupleEquals(rhs.asTuple)
      case rhs: Point3D => tupleEquals(rhs.asTuple)
      case _ => false
    }
  }

  @inline protected def tupleEquals(rhs: Tuple4): Boolean = {
    abs(x - rhs.x) < EPSILON_TEST && abs(y - rhs.y) < EPSILON_TEST && abs(z - rhs.z) < EPSILON_TEST
  }
}
