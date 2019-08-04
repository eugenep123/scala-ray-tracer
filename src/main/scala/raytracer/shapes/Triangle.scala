package raytracer
package shapes

import math.abs

//https://en.wikipedia.org/wiki/M%C3%B6ller%E2%80%93Trumbore_intersection_algorithm

class Triangle(val p1: Point3D, val p2: Point3D, val p3: Point3D) extends MutableShape {
  val e1 = p2 - p1
  val e2 = p3 - p1
  val normal = e2.cross(e1).normalize

  override def localNormalAt(localPoint: Point3D, hit: Intersection): Vector3D = normal

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
      Seq(Intersection(t, this))
    }
  }
  override val bounds: BoundingBox = BoundingBox(p1, p2, p3)
}

object Triangle {

  def apply(
    p1: Point3D,
    p2: Point3D,
    p3: Point3D,
    transform: Matrix = Matrix.identity,
    material: Material = Material(),
    parent: Option[Shape] = None): Triangle = {
    new Triangle(p1, p2, p3).setTransform(transform).setMaterial(material).setParent(parent)
  }

  def fanTriangulation(vertices: Seq[Point3D]): Seq[Triangle] = {
    (1 until vertices.size - 1).map(index => Triangle(vertices(0), vertices(index), vertices(index + 1)))
  }


}