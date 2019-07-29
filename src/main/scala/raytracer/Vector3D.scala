package raytracer

import java.lang.Math.{abs, pow, sqrt}

final case class Vector3D(x: Double, y: Double, z: Double) {
  def w: Double = 0.0
  def +(v: Vector3D): Vector3D = Vector3D(x + v.x, y + v.y, z + v.z)
  def -(v: Vector3D): Vector3D = Vector3D(x - v.x, y - v.y, z - v.z)
  def *(s: Double): Vector3D = Vector3D(x * s, y * s, z * s)
  def /(s: Double): Vector3D = Vector3D(x / s, y / s, z / s)
  def unary_- = Vector3D(-x, -y, -z)
  def dot(v: Vector3D): Double = x * v.x + y * v.y + z * v.z
  def magnitude: Double = sqrt(pow(x, 2) + pow(y, 2) + pow(z, 2))
  def normalize: Vector3D = {
    val m = this.magnitude
    require(m > 0)
    Vector3D(x / m, y / m, z / m)
  }
  def cross(other: Vector3D): Vector3D = {
    Vector3D(
      this.y * other.z - this.z * other.y,
      this.z * other.x - this.x * other.z,
      this.x * other.y - this.y * other.x
    )
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case rhs: Vector3D => Vector3D.isEquals(this, rhs)
      case rhs: Tuple4 => Tuple4.isEquals(this.asTuple, rhs)
      case _ => false
    }
  }

  override def toString: String =  mkString("vector", x, y, z, w)

  def asTuple: Tuple4 = Tuple4(x, y, z, w)

}

object Vector3D {
  @inline final def isEquals(lhs: Vector3D, rhs: Vector3D): Boolean = {
    abs(lhs.x - rhs.x) < EPSILON_TEST &&
    abs(lhs.y - rhs.y) < EPSILON_TEST &&
    abs(lhs.z - rhs.z) < EPSILON_TEST
  }

  @inline final def reflect(in: Vector3D, normal: Vector3D): Vector3D = {
    in - normal * 2 * in.dot(normal)
  }
}