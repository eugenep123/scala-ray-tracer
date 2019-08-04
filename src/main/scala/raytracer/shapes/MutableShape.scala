package raytracer.shapes

import raytracer.{Intersection, Material, Matrix, Point3D, Ray, Shape, Vector3D}

abstract class MutableShape extends Shape {
  protected var _material: Material = Material.Default
  protected var _parent: Option[Shape] = None
  protected var _transform: Matrix = Matrix.identity
  protected var _renderAllRays: Boolean = true

  final override def transform: Matrix = this._transform
  final override def material: Material = this._material
  final override def parent: Option[Shape] = this._parent
  final override def renderAllRays: Boolean = _renderAllRays

  final override def setTransform(t: Matrix): this.type = {
    this._transform = t
    this
  }

  final override def setParent(parent: Option[Shape]): this.type = {
    this._parent = parent
    //TODO: recalc transform
    this
  }

  final override def setMaterial(m: Material): this.type = {
    this._material = m
    this
  }

  final override def setRenderAllRays(enabled: Boolean): this.type = {
    this._renderAllRays = enabled
    this
  }


  final def intersect(ray: Ray): Seq[Intersection] = {
    val localRay = ray.transform(transform.inverse)
    localIntersect(localRay)
  }

  //lazy val worldToObjectTransformMatrix: Matrix = this.worldToObjectTransform
//  override def worldToObjectTransform: Matrix = {
//    parent.fold(transform.inverse)(p => transform.inverse * p.worldToObjectTransform)
//  }

  final override def normalAt(point: Point3D, hit: Intersection): Vector3D = {
    val localPoint = worldToObject(point)
    val localNormal = localNormalAt(localPoint, hit)
    normalToWorld(localNormal)
  }

  final override def worldToObject(point: Point3D): Point3D = {
    val p = parent.fold(point)(_.worldToObject(point))
    transform.inverse * p
  }

  override def normalToWorld(normal: Vector3D): Vector3D = {
    val n = (transform.inverse.transpose * normal).normalize
    parent.fold(n)(_.normalToWorld(n))
  }

  def localNormalAt(localPoint: Point3D, hit: Intersection): Vector3D
  def localIntersect(ray: Ray): Seq[Intersection]

  override def equals(obj: Any): Boolean = {
    obj match {
      case s: Shape =>
        s.transform == transform &&
        s.parent == parent &&
        s.material == material
      case _ => false
    }
  }

}


