package raytracer.shapes

import raytracer.math._

import scala.math.abs

// Shared triangle data structure (only triangle data)
// Can be shared between shapes
sealed trait TriangleData {
  def p1: Point3D
  def p2: Point3D
  def p3: Point3D
  def bounds: BoundingBox
  def localNormalAt(localPoint: Point3D, hit: Intersection): Vector3D
  def localIntersect(shape: Shape, ray: Ray): Seq[Intersection]
}
object TriangleData {

  def apply(p1: Point3D, p2: Point3D, p3: Point3D): TriangleData = Default(p1, p2, p3)
  def apply(
    p1: Point3D, p2: Point3D, p3: Point3D,
    n1: Vector3D, n2: Vector3D, n3: Vector3D): TriangleData = Smooth(p1, p2, p3, n1, n2, n3)

  case class Default(p1: Point3D, p2: Point3D, p3: Point3D) extends TriangleData {
    val e1: Vector3D = p2 - p1
    val e2: Vector3D = p3 - p1
    val normal: Vector3D = e2.cross(e1).normalize
    val bounds: BoundingBox = BoundingBox.of(p1, p2, p3)

    override def localNormalAt(localPoint: Point3D, hit: Intersection): Vector3D = normal

    override def localIntersect(shape: Shape, ray: Ray): Seq[Intersection] = {
      intersect(shape, ray, p1, e1, e2)
    }
  }

  case class Smooth(
    p1: Point3D, p2: Point3D, p3: Point3D,
    n1: Vector3D, n2: Vector3D, n3: Vector3D) extends TriangleData {
    val e1: Vector3D = p2 - p1
    val e2: Vector3D = p3 - p1
    val bounds: BoundingBox = BoundingBox.of(p1, p2, p3)

    override def localNormalAt(localPoint: Point3D, hit: Intersection): Vector3D = {
      n2 * hit.u + n3 * hit.v + n1 * (1 - hit.u - hit.v)
    }

    override def localIntersect(shape: Shape, ray: Ray): Seq[Intersection] =
      intersect(shape, ray, p1, e1, e2)
  }


  // https://en.wikipedia.org/wiki/M%C3%B6ller%E2%80%93Trumbore_intersection_algorithm
  @inline def intersect(
    shape: Shape,
    ray: Ray,
    p1: Point3D,
    e1: Vector3D,
    e2: Vector3D): Seq[Intersection] = {
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
      Seq(Intersection(t, shape, u, v))
    }
  }


  def fanTriangulation(vertices: Seq[Point3D]): Seq[TriangleData] = {
    (1 until vertices.size - 1).map(index => TriangleData(vertices(0), vertices(index), vertices(index + 1)))
  }

  def fanTriangulationS(pairs: Seq[(Point3D, Vector3D)]): Seq[TriangleData] = {
    (1 until pairs.size - 1).map { index =>
      val p1 = pairs(0)
      val p2 = pairs(index)
      val p3 = pairs(index + 1)
      TriangleData(p1._1, p2._1, p3._1, p1._2, p2._2, p3._2)
    }
  }
}