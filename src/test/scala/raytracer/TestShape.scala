package raytracer

import raytracer.shapes.MutableShape


class TestShape extends MutableShape {

  var savedRay: Ray = _

  override def localIntersect(ray: Ray): Seq[Intersection] = {
    this.savedRay = ray
    Nil
  }

  override def localNormalAt(p: Point3D): Vector3D = {
    Vector3D(p.x, p.y, p.z)
  }

  override def bounds: BoundingBox =
    BoundingBox(Point3D(-1, -1, -1), Point3D(1, 1, 1))

}

object TestShape {
  def apply(
    transform: Matrix = Matrix.identity,
    material: Material = Material(),
    parent: Option[Shape] = None): TestShape = {
    new TestShape()
      .setTransform(transform)
      .setMaterial(material)
      .setParent(parent)
  }
}