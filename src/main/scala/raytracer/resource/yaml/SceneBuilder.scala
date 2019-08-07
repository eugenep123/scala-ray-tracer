package raytracer.resource.yaml

import AST._
import raytracer.math.{Matrix, Operation, Operations, TransformBuilder}
import raytracer.patterns._
import raytracer.{Camera, Defaults, Material, PointLight, Scene, World}
import raytracer.resource.ResourceLoader
import raytracer.shapes._

import scala.annotation.tailrec
import scala.reflect.ClassTag

case class SceneBuilder(items: Seq[YamlValue], divideThreshold: Int = 20)
                       (implicit loader: ResourceLoader) {

  implicit lazy val lookup = buildLookup

  protected def collect[A: ClassTag]: Seq[A] = items.collect { case a: A => a }

  def build: Scene = {
    val camera = collect[AddCamera].headOption.map(buildCamera).getOrElse(Defaults.camera)
    val pointLights = collect[AddLight].map(buildLight)
    val objects = collect[AddShape].map(buildShape)
    Scene(World(objects, pointLights), camera)
  }

  def buildShape(add: AddShape): Shape = {
    add match {
      case ShapeReference(key, t, m) =>
        val define = lookup.shapes.getOrElse(key, error(s"Shape reference not found: '$key'"))
        buildShapeValue(define.value, Some(key), t, m)
      case value: ShapeValue =>
        buildShapeValue(value, None, None, None)
    }
  }

  def buildShapeValue(
    value: ShapeValue,
    referenceKey: Option[String],
    parentTransform: TransformOption,
    parentMaterial: MaterialOption): Shape = {

    val transform = buildTransform(value.transform, parentTransform)
    val material = buildMaterial(value.material, parentMaterial)

    value match {
      case AddCone(min, max, closed, _, _) =>
        new Cone(min, max, closed, transform, material)
      case _: AddCube =>
        new Cube(transform, material)
      case AddCylinder(min, max, closed, _, _) =>
        new Cylinder(min, max, closed, transform, material)
      case g: AddGroup =>
        val group = new Group(transform, material) //Use optional group name
        val children = g.children.map(buildShape)
        children foreach group.addChild
        group
      case _: AddPlane =>
        new Plane(transform, material)
      case _: AddSphere =>
        new Sphere(transform, material)
      case obj: AddObjFile =>
        buildObjFile(obj.file, transform, material)
    }
  }

  def buildObjFile(filename: String, transform: Matrix, material: Option[Material]): Shape = {
    val group = loader.loadObject(filename) // no transforms
    val group2 = new Group(transform, material)
    group2.addChildren(group.children)
    // subdivide bounding boxes
    group2.divide(divideThreshold)
    group2
  }

  def buildCamera(add: AddCamera): Camera = {
    import add._
    Camera(width, height, fieldOfView, Operations.viewTransform(from, to, up))
  }

  def buildLight(add: AddLight): PointLight = {
    PointLight(add.at, add.intensity)
  }

  def buildPattern(p: PatternValue): Pattern = {
    val transform = buildTransform(p.transform, None)
    p match {
      case CheckersPatternValue(a, b, _)  => CheckersPattern(a, b, transform)
      case StripePatternValue(a, b, _)    => StripePattern(a, b, transform)
      case GradientPatternValue(a, b, _)  => GradientPattern(a, b, transform)
      case RingPatternValue(a, b, _)      => RingPattern(a, b, transform)
    }
  }

  def buildTransform(transform: TransformOption, parent: TransformOption): Matrix = {
    val xs = transform.fold(Seq.empty[TransformItem])(_.xs) ++
      parent.fold(Seq.empty[TransformItem])(_.xs)
    val ops = xs.flatMap(resolveTransformItem)
    ops.foldLeft(TransformBuilder.apply())((b, op) => b.add(op)).build()
  }

  def resolveTransformItem(t: TransformItem): Seq[Operation] = {
    t match {
      case TransformOperation(op) => Seq(op)
      case TransformReference(key) =>
        val define = lookup.transforms.getOrElse(key, error(s"Transform not found: '$key'"))
        val items = define.value.xs
        items flatMap resolveTransformItem
    }
  }


  def buildMaterial(material: MaterialOption, parent: MaterialOption): Option[Material] = {
    val xs = resolveMaterialList(material) ++ resolveMaterialList(parent)
    val merged = mergeMaterial(xs)
    buildMaterial(merged)
  }

  def buildMaterial(key: String): Option[Material] = {
    buildMaterial(Some(MaterialReference("blue-material")), None)
  }
  def buildMaterial(m: MaterialObject): Option[Material] = {
    val isEmpty = m.productIterator.forall { case o: Option[_] => o.isEmpty }
    if (isEmpty) None
    else {
      val default = Material.Default
      val pattern = m.pattern.map(buildPattern).orElse(default.pattern)
      val mat = new Material(
        m.color.getOrElse(default.color),
        m.ambient.getOrElse(default.ambient),
        m.diffuse.getOrElse(default.diffuse),
        m.specular.getOrElse(default.specular),
        m.shininess.getOrElse(default.shininess),
        m.reflective.getOrElse(default.reflective),
        m.transparency.getOrElse(default.transparency),
        m.refractiveIndex.getOrElse(default.refractiveIndex),
        pattern
      )
      Some(mat)
    }
  }


  def mergeMaterial(xs: Seq[MaterialObject]): MaterialObject = {
    val empty = MaterialObjectBuilder()
    xs.foldLeft(empty)((builder, obj) => builder.set(obj)).obj
  }

  def resolveMaterial(key: String): MaterialObject = {
    mergeMaterial(resolveMaterialList(key))
  }

  def resolveMaterialList(opt: MaterialOption): Seq[MaterialObject] = {
    opt.fold(Seq.empty[MaterialObject])(resolveMaterialList)
  }

  def resolveMaterialList(value: MaterialValue): Seq[MaterialObject] = {
    value match {
      case MaterialReference(key) => resolveMaterialList(key)
      case obj: MaterialObject => Seq(obj)
    }
  }

  def resolveMaterialList(key: String): Seq[MaterialObject] = {
    @tailrec def get(opt: Option[String], acc: List[MaterialObject]): List[MaterialObject] = {
      opt match {
        case None => acc
        case Some(key) =>
          val mat = lookup.materials.getOrElse(key, error(s"Material not found: '$key'"))
          get(mat.extend, mat.value :: acc)
      }
    }
    get(Some(key), Nil)
  }

  def buildLookup: DefineLookup = {
    def toMap[A <: DefineValue](xs: Seq[A]) = xs.map(x => x.key -> x).toMap
    val materials = collect[DefineMaterial]
    val transforms = collect[DefineTransform]
    val shapes = collect[DefineShape]
    DefineLookup(toMap(materials), toMap(transforms), toMap(shapes))
  }

  private def error(msg: String): Nothing = throw new RuntimeException(msg)
}
