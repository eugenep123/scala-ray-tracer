package raytracer.shapes

import raytracer.Defaults.Materials
import raytracer.math._
import raytracer._
import patterns._

case class ShapeBuilder(
  transform: Matrix = Matrix.identity,
  materialOpt: Option[Material] = None) {

  def material: Material = materialOpt.getOrElse(Material())

  def setTransform(m: Matrix): ShapeBuilder =
    new ShapeBuilder(m, materialOpt)
  def transform(f: TransformBuilder => TransformBuilder): ShapeBuilder = {
    setTransform(f(TransformBuilder.from(transform)).build())
  }
  def translate(x: Double, y: Double, z: Double): ShapeBuilder =
    transform(tb => tb.translate(x, y, z))
  def scale(n: Double): ShapeBuilder =
    transform(tb => tb.scale(n))
  def scale(x: Double, y: Double, z: Double): ShapeBuilder =
    transform(_.scale(x, y, z))
  def rotateX(radians: Radians): ShapeBuilder =
    transform(_.rotateX(radians))
  def rotateY(radians: Radians): ShapeBuilder =
    transform(_.rotateY(radians))
  def rotateZ(radians: Radians): ShapeBuilder =
    transform(_.rotateZ(radians))

  def setMaterial(m: Material): ShapeBuilder =
    new ShapeBuilder(transform, Some(m))

  final def updateMaterial(f: Material => Material): ShapeBuilder = setMaterial(f(material))
  final def setPattern(p: Pattern): ShapeBuilder = updateMaterial(_.setPattern(p))
  final def setReflective(r: Double): ShapeBuilder = updateMaterial(_.setReflective(r))
  final def setRefractiveIndex(i: Double): ShapeBuilder = updateMaterial(_.setRefractiveIndex(i))
  final def setTransparency(t: Double): ShapeBuilder = updateMaterial(_.setTransparency(t))
  final def setAmbient(a: Double): ShapeBuilder = updateMaterial(_.setAmbient(a))
  final def setColor(c: Color): ShapeBuilder = updateMaterial(_.setColor(c))
  final def setColor(r: Double, g: Double, b: Double): ShapeBuilder = setColor(Color(r, g, b))
  final def glass: ShapeBuilder = setMaterial(Materials.glass)

  def cone(
    minimum: Double = -INFINITY,
    maximum: Double = INFINITY,
    closed: Boolean = false): Cone =
    new Cone(minimum, maximum, closed, transform, materialOpt)

  def cylinder(
    minimum: Double = -INFINITY,
    maximum: Double = INFINITY,
    closed: Boolean = false): Cylinder =
    new Cylinder(minimum, maximum, closed, transform, materialOpt)

  def cube: Cube = new Cube(transform, materialOpt)

  def smoothTriangle(
    p1: Point3D, p2: Point3D, p3: Point3D,
    n1: Vector3D, n2: Vector3D, n3: Vector3D): SmoothTriangle =
    new SmoothTriangle(p1, p2, p3, n1, n2, n3, transform, materialOpt)
  def triangle(
    p1: Point3D, p2: Point3D, p3: Point3D): Triangle =
    new Triangle(p1, p2, p3, transform, materialOpt)
  def plane: Plane = new Plane(transform, materialOpt)
  def sphere: Sphere = new Sphere(transform, materialOpt)
  def testShape: TestShape = new TestShape(transform, materialOpt)

  def group: Group = new Group(transform, materialOpt)
  def group(child: Shape, more: Shape*): Group = {
    group(Seq(child) ++ more)
  }

  def group(children: Seq[Shape]): Group = {
    val g = group
    children foreach g.addChild
    g
  }

  def groupFill(n: Int)(f: Int => Shape): Group = {
    group((0 until n).map(f))
  }

  def buildOfType(shape: Shape): Shape = shape match {
    case _: Cube => cube
    case c: Cone => cone(c.minimum, c.maximum, c.closed)
    case c: Cylinder => cylinder(c.minimum, c.maximum, c.closed)
    case _: Group => group
    case _: Plane => plane
    case t: Triangle => triangle(t.p1, t.p2, t.p3)
    case t: SmoothTriangle => smoothTriangle(t.p1, t.p2, t.p3, t.n1, t.n2, t.n3)
    case _: Sphere => sphere
    case _: TestShape => testShape
  }
}

object ShapeBuilder {
  def apply(): ShapeBuilder = new ShapeBuilder()
}
