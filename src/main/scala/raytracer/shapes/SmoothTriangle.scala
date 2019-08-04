package raytracer
package shapes

import scala.math.abs


class SmoothTriangle(
  val p1: Point3D, val p2: Point3D, val p3: Point3D,
  val n1: Vector3D, val n2: Vector3D, val n3: Vector3D) extends MutableShape {

  val e1 = p2 - p1
  val e2 = p3 - p1
  val normal = e2.cross(e1).normalize

  override def localNormalAt(localPoint: Point3D, hit: Intersection): Vector3D = {
    n2 * hit.u + n3 * hit.v + n1 * (1 - hit.u - hit.v)
  }

  override def localIntersect(ray: Ray): Seq[Intersection] = {
    val dirCrossE2 = ray.direction.cross(e2)
    val det = e1.dot(dirCrossE2)
    if (abs(det) < EPSILON) Nil
    else {
      // Check p1-p3 edge
      val f = 1.0 / det
      val p1ToOrigin = ray.origin - p1
      val u = f * p1ToOrigin.dot(dirCrossE2)
      if (u < 0.0 || u > 1.0) return Nil

      // p1-p2 and p2-p3
      val originCrossE1 = p1ToOrigin.cross(e1)
      val v = f * ray.direction.dot(originCrossE1)
      if (v < 0 || (u + v) > 1) return Nil

      val t = f * e2.dot(originCrossE1)
      Seq(Intersection(t, this, u, v))
    }
  }

  override val bounds: BoundingBox = BoundingBox(p1, p2, p3)
}

object SmoothTriangle {
  def apply(
    p1: Point3D, p2: Point3D, p3: Point3D,
    n1: Vector3D, n2: Vector3D, n3: Vector3D,
    transform: Matrix = Matrix.identity,
    material: Material = Material(),
    parent: Option[Shape] = None): SmoothTriangle = {
    new SmoothTriangle(p1, p2, p3, n1, n2, n3)
      .setTransform(transform)
      .setMaterial(material)
      .setParent(parent)
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