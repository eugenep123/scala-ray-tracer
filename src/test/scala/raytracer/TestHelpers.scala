package raytracer

import raytracer.math.{Intersection, Point3D, Ray, RayIntersection, Vector3D}
import raytracer.shapes.{Shape, Shapes}

trait TestHelpers extends Shapes {

  def tuple(x: Double, y: Double, z: Double, w: Double): math.Tuple4 = math.Tuple4(x, y, z, w)
  def point(x: Double, y: Double, z: Double): Point3D = Point3D(x, y, z)
  def vector(x: Double, y: Double, z: Double): Vector3D = Vector3D(x, y, z)

  def intersectionPairs(xs: (Double, Shape)*): Seq[Intersection] =
    xs.map { case (t, shape) => Intersection(t, shape) }
  def intersections(xs: Intersection*): Seq[Intersection] = Intersection.sort(xs)

  def normalAt(s: Shape, p: Point3D): Vector3D = s.normalAt(p, Intersection(0, s, 0, 0))

  def localNormalAt(s: Shape, p: Point3D): Vector3D = s.localNormalAt(p, null)

  def prepareComputations(i: Intersection, r: Ray, xs: Seq[Intersection] = Nil): RayIntersection = {
    val nonEmpty = if (xs.isEmpty) Seq(i) else xs //not to break tests
    if (xs.isEmpty) {
      println("WARN: intersections is empty!!!")
    }
    i.prepare(r, nonEmpty)
  }

  def defaultWorld(): World = Defaults.world

}
