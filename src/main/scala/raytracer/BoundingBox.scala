package raytracer


import Point3D._

//http://www.raytracerchallenge.com/bonus/bounding-boxes.html
case class BoundingBox(
  minimum: Point3D,
  maximum: Point3D) {

  def add(point: Point3D): BoundingBox =
    BoundingBox(min(minimum, point), max(maximum, point))

  def add(box: BoundingBox): BoundingBox =
    BoundingBox(min(minimum, box.minimum), max(maximum, box.maximum))
}


object BoundingBox {

  val Empty = new BoundingBox(
    Point3D(INFINITY, INFINITY, INFINITY),    // largest point
    Point3D(-INFINITY, -INFINITY, -INFINITY)  // smallest point
  )

  def apply(xs: Seq[Shape]): BoundingBox = {
    if (xs.isEmpty) Empty
    else {
      val minimum = ???
      val maximum = ???
      new BoundingBox(minimum, maximum)
    }
  }


}