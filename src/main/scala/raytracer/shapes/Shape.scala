//package raytracer.shapes
//
//import raytracer.{BoundingBox, Builder, Color, Intersection, Material, Matrix, Pattern, Point3D, Ray, TransformBuilder, Vector3D}
//
//trait Shape {
//
//  // Ignore bounds, shadows, reflection on debug shapes
//  def renderAllRays: Boolean
//  def parent: Option[Shape]
//  def transform: Matrix
//  def material: Material
//
//  //Helpers
//  def setTransform(t: Matrix): this.type
//  def setMaterial(m: Material): this.type
//  def setParent(p: Option[Shape]): this.type
//  def setRenderAllRays(enabled: Boolean): this.type
//
//  def intersect(ray: Ray): Seq[Intersection]
//  def normalAt(point: Point3D, hit: Intersection): Vector3D
//
//  def worldToObject(point: Point3D): Point3D
//  def normalToWorld(normal: Vector3D): Vector3D
//
//  // returns a bounding box in object space
//  def bounds: BoundingBox
//
////  def worldToObjectTransform: Matrix
//
//
//
//  // Helpers
//  final def updateMaterial(f: Material => Material): this.type = setMaterial(f(material))
//  final def setPattern(p: Pattern): this.type = updateMaterial(_.setPattern(p))
//  final def setReflective(r: Double): this.type = updateMaterial(_.setReflective(r))
//  final def setRefractiveIndex(i: Double): this.type = updateMaterial(_.setRefractiveIndex(i))
//  final def setTransparency(t: Double): this.type = updateMaterial(_.setTransparency(t))
//  final def setAmbient(a: Double): this.type = updateMaterial(_.setAmbient(a))
//  final def setColor(c: Color): this.type = updateMaterial(_.setColor(c))
//  final def setColor(r: Double, g: Double, b: Double): this.type = setColor(Color(r, g, b))
//
//  final def builtTransform(f: TransformBuilder => TransformBuilder): this.type =
//    setTransform(f(Builder))
//
//  final def path: List[Shape] = Shape.mkPath(this, Nil)
//}
//
//object Shape {
//  def mkPath(s: Shape, acc: List[Shape]): List[Shape] = {
//    s.parent.fold(s :: acc)(p => mkPath(p, s :: acc))
//  }
//}