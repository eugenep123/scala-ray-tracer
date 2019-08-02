package raytracer
package shapes

class TestShape extends MutableShape {

  private var _savedRay = Option.empty[Ray]

  override def localIntersect(ray: Ray): Seq[Intersection] = {
    this._savedRay = Some(ray)
    Nil
  }

  override def localNormalAt(p: Point3D): Vector3D = {
    Vector3D(p.x, p.y, p.z)
  }

  override def bounds: BoundingBox =
    BoundingBox(Point3D(-1, -1, -1), Point3D(1, 1, 1))

  def savedRay: Option[Ray] = _savedRay
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