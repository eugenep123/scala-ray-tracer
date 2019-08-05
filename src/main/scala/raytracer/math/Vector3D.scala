package raytracer.math

import java.lang.Math.{pow, sqrt}

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
  def abs: Vector3D = Vector3D(math.abs(x), math.abs(y), math.abs(z))

  override def equals(obj: Any): Boolean = {
    obj match {
      case rhs: Vector3D =>
        math.abs(x - rhs.x) < EPSILON_TEST &&
        math.abs(y - rhs.y) < EPSILON_TEST &&
        math.abs(z - rhs.z) < EPSILON_TEST
      case rhs: Tuple4 => rhs.equals(this.asTuple)
      case _ => false
    }
  }

  override def toString: String =  mkString("vector", x, y, z, w)

  def asTuple: Tuple4 = Tuple4(x, y, z, w)
}

object Vector3D {

  @inline final def reflect(in: Vector3D, normal: Vector3D): Vector3D = {
    in - normal * 2 * in.dot(normal)
  }
}