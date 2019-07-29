package raytracer

import raytracer.Tuple4._

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
      case rhs: Tuple4 => isEquals(this, rhs)
      case rhs: Vector3D => isEquals(this, rhs.asTuple)
      case rhs: Point3D => isEquals(this, rhs.asTuple)
      case _ => false
    }
  }
}

object Tuple4 {
  @inline final def isEquals(lhs: Tuple4, rhs: Tuple4): Boolean = {
    abs(lhs.x - rhs.x) < EPSILON_TEST &&
    abs(lhs.y - rhs.y) < EPSILON_TEST &&
    abs(lhs.z - rhs.z) < EPSILON_TEST
  }
}