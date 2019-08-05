package raytracer
package shapes

import math._

class SmoothTriangle(
  p1: Point3D, p2: Point3D, p3: Point3D,
  val n1: Vector3D, val n2: Vector3D, val n3: Vector3D,
  transform: Matrix = Matrix.identity,
  material: Option[Material] = None) extends Triangle(p1, p2, p3, transform, material) {

  override def localNormalAt(localPoint: Point3D, hit: Intersection): Vector3D = {
    n2 * hit.u + n3 * hit.v + n1 * (1 - hit.u - hit.v)
  }
  override def canEqual(other: Any): Boolean = other.isInstanceOf[SmoothTriangle]

  override def hashCode: Int = {
    (super.hashCode, p1, p2, p3, n1, n2, n3).hashCode()
  }
}

object SmoothTriangle {
  def apply(
    p1: Point3D, p2: Point3D, p3: Point3D,
    n1: Vector3D, n2: Vector3D, n3: Vector3D,
    transform: Matrix = Matrix.identity,
    material: Option[Material] = None): SmoothTriangle = {
    new SmoothTriangle(p1, p2, p3, n1, n2, n3, transform, material)
  }

  def fanTriangulation(pairs: Seq[(Point3D, Vector3D)]): Seq[SmoothTriangle] = {
    (1 until pairs.size - 1).map { index =>
      val p1 = pairs(0)
      val p2 = pairs(index)
      val p3 = pairs(index + 1)
      SmoothTriangle(p1._1, p2._1, p3._1, p1._2, p2._2, p3._2)
    }
  }
}