package raytracer

import raytracer.files.{ObjFileParser, PpmWriter}
import raytracer.patterns.{CheckersPattern, GradientPattern, RingPattern, StripePattern}
import raytracer.shapes.{Group, TestShape}

trait TestHelpers extends Shapes {
  import Defaults.MaxRecursion

  // Canvas
  def canvas(width: Int, height: Int): Canvas = Canvas(width, height)
  def writePixel(c: Canvas, x: Int, y: Int, color: Color): Unit = c.writePixel(x, y, color)
  def pixelAt(c: Canvas, x: Int, y: Int): Color = c(x, y)
  def canvasToPpm(c: Canvas): Seq[String] = PpmWriter(c)

  def tuple(x: Double, y: Double, z: Double, w: Double): Tuple4 = Tuple4(x, y, z, w)
  def point(x: Double, y: Double, z: Double): Point3D = Point3D(x, y, z)
  def vector(x: Double, y: Double, z: Double): Vector3D = Vector3D(x, y, z)
  def color(red: Double, green: Double, blue: Double): Color = Color(red, green, blue)

  def magnitude(v: Vector3D): Double = v.magnitude
  def normalize(v: Vector3D): Vector3D = v.normalize
  def dot(a: Vector3D, b: Vector3D): Double = a dot b
  def cross(a: Vector3D, b: Vector3D): Vector3D = a cross b
  def reflect(in: Vector3D, normal: Vector3D): Vector3D =
    Vector3D.reflect(in, normal)

  def transpose(m: Matrix): Matrix = m.transpose
  def determinant(m: Matrix): Double = m.determinant
  def inverse(m: Matrix): Matrix = m.inverse
  def identityMatrix: Matrix = Matrix.identity

  def ray(origin: Point3D, direction: Vector3D): Ray = Ray(origin, direction)
  def position(ray: Ray, t: Double): Point3D = ray.position(t)
  def transform(ray: Ray, m: Matrix): Ray = ray.transform(m)

  // Rendering
  def intersect(s: Shape, r: Ray): Seq[Intersection] = s.intersect(r)
  def intersection(t: Double, s: Shape): Intersection = Intersection(t, s)
  def intersectionPairs(xs: (Double, Shape)*): Seq[Intersection] =
    xs.map { case (t, shape) => Intersection(t, shape) }
  def intersections(xs: Intersection*): Seq[Intersection] = Intersection.sort(xs)
  def hit(xs: Seq[Intersection]): Option[Intersection] = Intersection.hit(xs)
  def setTransform[S <: Shape](s: S, t: Matrix): S = s.setTransform(t).asInstanceOf[S]
  def normalAt(s: Shape, p: Point3D): Vector3D = s.normalAt(p)
  def localNormalAt(s: Shape, p: Point3D): Vector3D = s.localNormalAt(p)
  def localIntersect(s: Shape, r: Ray): Seq[Intersection] = s.localIntersect(r)
  def pointLight(position: Point3D, intensity: Color): PointLight = PointLight(position, intensity)
  def material(): Material = Material()

  def lighting(
    material: Material,
    light: PointLight,
    point: Point3D,
    eye: Vector3D,
    normal: Vector3D,
    inShadow: Boolean = false): Color =
    material.lighting(light, point, eye, normal, inShadow)

  def isShadowed(world: World, p: Point3D): Boolean = world.isShadowed(p)
  def intersectWorld(w: World, r: Ray): Seq[Intersection] = w.intersect(r)
  def prepareComputations(i: Intersection, r: Ray, xs: Seq[Intersection] = Nil): RayIntersection = {
    val nonEmpty = if (xs.isEmpty) Seq(i) else xs //not to break tests
    if (xs.isEmpty) {
      println("WARN: intersections is empty!!!")
    }
    i.prepare(r, nonEmpty)
  }
  def shadeHit(world: World, comps: RayIntersection, remaining: Int = MaxRecursion): Color =
    world.shadeHit(comps, remaining).getOrElse(Color.Black)
  def colorAt(w: World, r: Ray, remaining: Int = MaxRecursion): Color = w.colorAtOrBlack(r, remaining)

  def reflectedColor(world: World, hit: RayIntersection, remaining: Int = MaxRecursion): Color =
    world.reflectedColor(hit, remaining).getOrElse(Color.Black)

  def refractedColor(world: World, hit: RayIntersection, remaining: Int = MaxRecursion): Color =
    world.refractedColor(hit, remaining).getOrElse(Color.Black)

  def defaultWorld(): World = Defaults.world
  def testPattern(): Pattern = new TestPattern()

  def camera(width: Int, height: Int, fieldOfView: Double): Camera = Camera(width, height, fieldOfView)
  def rayForPixel(c: Camera, x: Int, y: Int): Ray = c.rayForPixel(x, y)
  def render(c: Camera, w: World): Canvas = c.renderToCanvas(w)

  def schlick(hit: RayIntersection): Double = {
    hit.schlick()
  }

  def worldToObject(s: Shape, point: Point3D): Point3D = s.worldToObject(point)
  def normalToWorld(s: Shape, normal: Vector3D): Vector3D = s.normalToWorld(normal)

  def testShape(): TestShape = TestShape(identityMatrix, material())

  def addChild(g: Group, child: Shape): Group = g.add(child)

  def boundingBox(shapes: Seq[Shape] = Nil): BoundingBox = BoundingBox(shapes)
  def boundingBox(min: Point3D, max: Point3D): BoundingBox = new BoundingBox(min, max)


  def parseObjFile(content: String): ObjFileParser.Result = ObjFileParser.parse(content)

  //Patterns
  def stripePattern(a: Color, b: Color): StripePattern =
    StripePattern(a, b)
  def gradientPattern(a: Color, b: Color): GradientPattern = GradientPattern(a, b)
  def checkersPattern(a: Color, b: Color): CheckersPattern = CheckersPattern(a, b)
  def ringPattern(a: Color, b: Color): RingPattern = RingPattern(a, b)
  def patternAt(pattern: Pattern, point: Point3D): Color = pattern.colorAt(point)
  def patternAtShape(pattern: Pattern, shape: Shape, point: Point3D): Color =
    pattern.colorAt(point, shape)

  // Transforms
  val identity = Operation.Identity
  val translation = Operation.Translation.apply _
  val scaling = Operation.Scaling.apply _
  val rotationX = Operation.RotationX.apply _
  val rotationY = Operation.RotationY.apply _
  val rotationZ = Operation.RotationZ.apply _
  val shearing = Operation.Shearing.apply _
  val viewTransform = Operation.ViewTransform.apply _
}
