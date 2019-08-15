package raytracer.math

import java.lang.Math.abs

import raytracer.Color
import raytracer.shapes.BoundingBox

final case class Point3D(x: Double, y: Double, z: Double) {
  def w: Double = 1.0

  def +(v: Vector3D): Point3D = Point3D(x + v.x, y + v.y, z + v.z)
  def -(p: Point3D): Vector3D = Vector3D(x - p.x, y - p.y, z - p.z)
  def -(v: Vector3D): Point3D = Point3D(x - v.x, y - v.y, z - v.z)
  def unary_- = Point3D(-x, -y, -z)

//  def *(s: Double): Point3D = Point3D(x * s, y * s, z * s)
//  def +(p: Point3D): Point3D = Point3D(x + p.x, y + p.y, z + p.z)

  override def equals(other: Any): Boolean = other match {
    case rhs: Point3D =>
      abs(x - rhs.x) < EPSILON_TEST &&
      abs(y - rhs.y) < EPSILON_TEST &&
      abs(z - rhs.z) < EPSILON_TEST
    case rhs: Tuple4 =>
      asTuple.equals(rhs)
    case _ => false
  }

  def asTuple: Tuple4 = Tuple4(x, y, z, w)
  def asColor: Color = Color(x, y, z)
  override def toString: String = mkString("point", x, y, z, w)
}

object Point3D {
  val origin = Point3D(0, 0, 0)

  @inline def min(a: Point3D, b: Point3D): Point3D =
    Point3D(math.min(a.x, b.x), math.min(a.y, b.y), math.min(a.z, b.z))

  @inline def max(a: Point3D, b: Point3D): Point3D =
    Point3D(math.max(a.x, b.x), math.max(a.y, b.y), math.max(a.z, b.z))

  @inline def center(a: Point3D, b: Point3D): Point3D =
    Point3D((a.x + b.x) * 0.5, (a.y + b.y) * 0.5, (a.z + b.z) * 0.5)

  def normalize(xs: Vector[Point3D]): Vector[Point3D] = {
    val bounds = BoundingBox.of(xs)
    val Vector3D(sx, sy, sz) = bounds.diagonal
    val scale = raytracer.math.max(sx, sy, sz) / 2.0
    val min = bounds.minimum
    xs.map { v =>
      Point3D(
        (v.x - (min.x + sx / 2)) / scale,
        (v.y - (min.y + sy / 2)) / scale,
        (v.z - (min.z + sz / 2)) / scale
      )
    }
  }
}
