package raytracer

import Point3D._
import raytracer.shapes.Cube
import raytracer.shapes.Cube.checkAxis

//http://www.raytracerchallenge.com/bonus/bounding-boxes.html
final case class BoundingBox(
  minimum: Point3D,
  maximum: Point3D) {

  def center: Point3D = Point3D.center(minimum, maximum)
  def extents: Vector3D = (minimum - maximum).abs / 2.0

  def transform(m: Matrix): BoundingBox = {
    val points = Seq(
      maximum,
      Point3D(minimum.x, minimum.y, maximum.z),
      Point3D(minimum.x, maximum.y, minimum.z),
      Point3D(minimum.x, maximum.y, maximum.z),
      Point3D(maximum.x, minimum.y, minimum.z),
      Point3D(maximum.x, minimum.y, maximum.z),
      Point3D(maximum.x, maximum.y, minimum.z),
      maximum
    )
    points.foldLeft(BoundingBox.Empty)((b, p) => b.add(m * p))
  }

  def intersects(ray: Ray): Boolean = {
    import ray.{direction, origin}
    val (xtmin, xtmax) = checkAxis(origin.x, direction.x, minimum.x, maximum.x)
    val (ytmin, ytmax) = checkAxis(origin.y, direction.y, minimum.y ,maximum.y)
    val (ztmin, ztmax) = checkAxis(origin.z, direction.z, minimum.z, maximum.z)

    val tmin = math.max(math.max(xtmin, ytmin), ztmin)
    val tmax = math.min(math.min(xtmax, ytmax), ztmax)
    (tmin <= tmax)
  }

  def contains(point: Point3D): Boolean = {
    point.x >= minimum.x && point.x <= maximum.x &&
    point.y >= minimum.y && point.y <= maximum.y &&
    point.z >= minimum.z && point.z <= maximum.z
  }

  def contains(b: BoundingBox): Boolean =
    contains(b.minimum) && contains(b.maximum)

  def add(point: Point3D): BoundingBox =
    BoundingBox(min(minimum, point), max(maximum, point))

  def add(box: BoundingBox): BoundingBox =
    BoundingBox(min(minimum, box.minimum), max(maximum, box.maximum))

  def toCube: Cube = {
    val toOrigin = Point3D.origin - this.center
    val ext = this.extents
    val scale = Vector3D(1.0 / ext.x, 1.0 / ext.y, 1.0 / ext.z)
    val transform = Builder.translate(toOrigin).scale(scale).inverse
    Cube(transform, Defaults.Materials.boundingBoxMaterial)
  }

}


object BoundingBox {

  val Empty = new BoundingBox(
    Point3D(INFINITY, INFINITY, INFINITY),    // largest point
    Point3D(-INFINITY, -INFINITY, -INFINITY)  // smallest point
  )

  def apply(shapes: Seq[Shape]): BoundingBox = {
    if (shapes.isEmpty) Empty
    else {
      shapes.foldLeft(Empty)((box, shape) => box.add(shape.bounds.transform(shape.transform)))
    }
  }

  @inline def apply(p1: Point3D, p2: Point3D, p3: Point3D): BoundingBox = {
    Empty.add(p1).add(p2).add(p3)
  }

}