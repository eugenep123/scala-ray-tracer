package raytracer.shapes

import raytracer.Material
import raytracer.math.{Intersection, Matrix, Point3D, Ray, Vector3D}

class Triangle(
  val data: TriangleData,
  transform: Matrix,
  material: Option[Material]) extends Shape(transform, material) {

  def p1: Point3D = data.p1
  def p2: Point3D = data.p2
  def p3: Point3D = data.p3

  override protected def calculateBounds: BoundingBox = data.bounds

  override def localNormalAt(localPoint: Point3D, hit: Intersection): Vector3D =
    data.localNormalAt(localPoint, hit)

  override def localIntersect(ray: Ray): Seq[Intersection] =
    data.localIntersect(this, ray)

  override def canEqual(other: Any): Boolean =
    other.isInstanceOf[Triangle]

  override def hashCode: Int = {
    (super.hashCode, data.hashCode()).hashCode()
  }
}

object Triangle {
  def apply(
    p1: Point3D,
    p2: Point3D,
    p3: Point3D,
    transform: Matrix = Matrix.identity,
    material: Option[Material] = None): Triangle =
    new Triangle(TriangleData(p1, p2, p3), transform, material)

  def smooth(
    p1: Point3D, p2: Point3D, p3: Point3D,
    n1: Vector3D, n2: Vector3D, n3: Vector3D,
    transform: Matrix = Matrix.identity,
    material: Option[Material] = None): Triangle = {
    new Triangle(TriangleData(p1, p2, p3, n1, n2, n3), transform, material)
  }

  def data(
    data: TriangleData,
    transform: Matrix = Matrix.identity,
    material: Option[Material] = None): Triangle = {
    new Triangle(data, transform, material)
  }
}