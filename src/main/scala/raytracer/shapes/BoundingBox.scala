package raytracer.shapes

import raytracer.math._
import Cube.checkAxis
import raytracer.{Defaults, Material}

//http://www.raytracerchallenge.com/bonus/bounding-boxes.html
final case class BoundingBox(
  minimum: Point3D,
  maximum: Point3D) {

  def center: Point3D = Point3D.center(minimum, maximum)
  def extents: Vector3D = (minimum - maximum).abs / 2.0
  def diagonal: Vector3D = (maximum - minimum)

//  def centroid = {
//    (minimum * 0.5) + (maximum * 0.5)
//  }
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
    BoundingBox.of(points.map(m * _))
  }

  def intersects(ray: Ray): Boolean = {
    import ray.{direction, origin}
    val (xtmin, xtmax) = checkAxis(origin.x, direction.x, minimum.x, maximum.x)
    val (ytmin, ytmax) = checkAxis(origin.y, direction.y, minimum.y ,maximum.y)
    val (ztmin, ztmax) = checkAxis(origin.z, direction.z, minimum.z, maximum.z)

    val tmin = math.max(math.max(xtmin, ytmin), ztmin)
    val tmax = math.min(math.min(xtmax, ytmax), ztmax)
    tmin <= tmax
  }

  def contains(point: Point3D): Boolean = {
    point.x >= minimum.x && point.x <= maximum.x &&
    point.y >= minimum.y && point.y <= maximum.y &&
    point.z >= minimum.z && point.z <= maximum.z
  }

  def contains(b: BoundingBox): Boolean =
    contains(b.minimum) && contains(b.maximum)

  def add(point: Point3D): BoundingBox =
    BoundingBox(Point3D.min(minimum, point), Point3D.max(maximum, point))

  def add(box: BoundingBox): BoundingBox =
    BoundingBox(Point3D.min(minimum, box.minimum), Point3D.max(maximum, box.maximum))

  def toCube: Cube = {
    val toOrigin = Point3D.origin - this.center
    val ext = this.extents
    val scale = Vector3D(1.0 / ext.x, 1.0 / ext.y, 1.0 / ext.z)
    val transform = Transform().translate(toOrigin).scale(scale).inverse
    Cube(transform, Material.TransparentBox)
  }

  def split: (BoundingBox, BoundingBox) = {
    // figure out the box's largest dimension
    val Vector3D(dx, dy, dz) = diagonal
    val greatest = max(dx, dy, dz)
    var (x0, y0, z0) = (minimum.x, minimum.y, minimum.z)
    var (x1, y1, z1) = (maximum.x, maximum.y, maximum.z)

    // adjust the points so that they lie on the dividing plane
    if (greatest == dx) {
      x1 = x0 + dx / 2.0
      x0 = x1
    } else if (greatest == dy) {
      y1 = y0 + dy / 2.0
      y0 = y1
    } else {
      z1 = z0 + dz / 2.0
      z0 = z1
    }

    val midMin = Point3D(x0, y0, z0)
    val midMax = Point3D(x1, y1, z1)

    // return the two halves of
    val left = BoundingBox(minimum, midMax)
    val right = BoundingBox(midMin, maximum)
    (left, right)
  }

}


object BoundingBox {

  val Empty = new BoundingBox(
    Point3D(INFINITY, INFINITY, INFINITY),    // largest point
    Point3D(-INFINITY, -INFINITY, -INFINITY)  // smallest point
  )

  def of(p1: Point3D, p2: Point3D, p3: Point3D): BoundingBox = {
    Empty.add(p1).add(p2).add(p3)
  }

  def of(xs: Seq[Point3D]): BoundingBox = {
    xs.foldLeft(Empty)((bbox, p) => bbox.add(p))
  }


}