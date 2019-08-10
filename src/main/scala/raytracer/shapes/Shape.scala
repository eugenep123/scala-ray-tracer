package raytracer
package shapes

import math._

/**
  * Shape is mostly immutable (except parent). You also cannot share shapes between groups
  *
  * @param transform
  * @param materialOpt
  */
abstract class Shape(
  val transform: Matrix,
  val materialOpt: Option[Material],
  val castsShadow: Boolean = true) {

  @volatile private var _bounds = Option.empty[BoundingBox]
  private var _parent = Option.empty[Shape]

  final def bounds: BoundingBox = {
    _bounds.getOrElse {
      val b = calculateBounds
      _bounds = Some(b)
      b
    }
  }

  final def material: Material = {
    materialOpt orElse(parent.map(_.material)) getOrElse Material.Default
  }

  final def parent: Option[Shape] = _parent
  final def parent_=(opt: Option[Shape]): Unit = {
    this._parent = opt
    this._bounds = None
  }
  final def setParent(p: Shape): this.type =
    setParent(Some(p))
  final def setParent(p: Option[Shape]): this.type = {
    this.parent = p
    //TODO: check if child is CSG not allowed to set to None
    this
  }

  // By default shapes do not have children (just check equality)
  def includes(s: Shape): Boolean = this eq s

  // Subdividing a primitive does nothing
  def divide(threshold: Int): Unit = {}

  // bounds in parent space
  final def parentSpaceBounds: BoundingBox = bounds.transform(transform)

  // bounds will be in object space
  protected def calculateBounds: BoundingBox

  final def intersect(ray: Ray): Seq[Intersection] = {
    val localRay = ray.transform(transform.inverse)
    localIntersect(localRay)
  }

  final def normalAt(point: Point3D, hit: Intersection): Vector3D = {
    val localPoint = worldToObject(point)
    val localNormal = localNormalAt(localPoint, hit)
    normalToWorld(localNormal)
  }

  def localNormalAt(localPoint: Point3D, hit: Intersection): Vector3D
  def localIntersect(ray: Ray): Seq[Intersection]


  final def worldToObject(point: Point3D): Point3D = {
    val p = parent.fold(point)(_.worldToObject(point))
    transform.inverse * p
  }

  final def normalToWorld(normal: Vector3D): Vector3D = {
    val n = (transform.inverse.transpose * normal).normalize
    parent.fold(n)(_.normalToWorld(n))
  }

  def canEqual(other: Any): Boolean

  final override def equals(other: Any): Boolean = {
    other match {
      case shape: Shape =>
        shape.canEqual(this) && this.hashCode == shape.hashCode
      case _ => false
    }
  }

  override def hashCode: Int = {
    31 + transform.hashCode() + materialOpt.hashCode()
  }

  final def cloneWith(f: ShapeBuilder => ShapeBuilder): Shape = {
    f(Shape.from(this)).buildOfType(this)
  }
}

object Shape {
  def apply(): ShapeBuilder = new ShapeBuilder()
  def from(s: Shape): ShapeBuilder = new ShapeBuilder(s.transform, s.materialOpt)

}

//
//trait NodeLike {
//  def isEmpty: Boolean
//  def includes(s: Shape): Boolean
//}