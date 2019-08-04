package raytracer
package yaml

import raytracer.files.ResourceLoader
import raytracer.patterns.{CheckersPattern, GradientPattern, StripePattern}
import raytracer.shapes.Group

package object ast {

  case class Reference(name: String, defineType: String) {
    def key: String = if (defineType == "shape") name else s"$name-$defineType"
  }

  sealed trait YamlItem
  sealed trait DefineObject extends YamlItem {
    def reference: Reference
    def key: String = reference.key
    def name: String = reference.name
  }

  case class DefineMaterial(
    reference: Reference,
    value: MaterialValue) extends DefineObject

  case class DefineTransform(
    reference: Reference,
    value: TransformList) extends DefineObject

  case class DefineShape(
    reference: Reference,
    value: ShapeObject) extends DefineObject

  case class TransformList(transforms: Seq[TransformObject]) {
    def buildOps(implicit lookup: DefineLookup): Seq[Operation] =
      transforms.flatMap(_.build)

    def build(implicit lookup: DefineLookup): Matrix = {
      buildOps
        .foldLeft(TransformBuilder.apply())((b, op) => b.add(op))
        .build()
    }
    def append(parent: Option[TransformList]): TransformList =
      parent.fold(this)(p => TransformList(transforms ++ p.transforms)) //TODO: check order

  }
  sealed trait TransformObject {
    def build(implicit lookup: DefineLookup): Seq[Operation]
  }
  case class TransformReference(reference: Reference) extends TransformObject {
    override def build(implicit lookup: DefineLookup): Seq[Operation] = {
      println(s"Resolving $reference")
      lookup.transforms.get(reference.key) match {
        case Some(t) =>
          t.value.buildOps
        case None =>
          println(s"reference not found: $reference!")
          Nil
      }
    }
  }
  case class TransformOperation(op: Operation) extends TransformObject {
    override def build(implicit lookup: DefineLookup): Seq[Operation] = Seq(op)
  }

  sealed trait MaterialObject {
    final def default: Material = Material()
    def build(implicit lookup: DefineLookup): Material
    def resolve(reference: Reference)(implicit lookup: DefineLookup): Material = {
      val material = lookup.materials.get(reference.key) match {
        case Some(define) => define.value.build
        case None =>
          println(s"reference not found: $reference!")
          default //default
      }
      println(s"Resolving material $reference to $material")
      material
    }
  }
  case class MaterialReference(reference: Reference) extends MaterialObject {
    override def build(implicit lookup: DefineLookup): Material = resolve(reference)
  }
  case class MaterialValue(
    extend: Option[Reference],
    color: Option[Color],
    diffuse: Option[Double],
    ambient: Option[Double],
    specular: Option[Double],
    shininess: Option[Double],
    reflective: Option[Double],
    transparency: Option[Double],
    refractiveIndex: Option[Double],
    pattern: Option[PatternValue]) extends MaterialObject {
    override def build(implicit lookup: DefineLookup): Material = {
      val base = extend.fold(default)(resolve)
      Material(
        color.getOrElse(base.color),
        ambient.getOrElse(base.ambient),
        diffuse.getOrElse(base.diffuse),
        specular.getOrElse(base.specular),
        shininess.getOrElse(base.shininess),
        reflective.getOrElse(base.reflective),
        transparency.getOrElse(base.transparency),
        refractiveIndex.getOrElse(base.refractiveIndex),
        pattern.map(_.build) orElse base.pattern
      )
    }
  }
  sealed trait PatternValue {
    def transform: TransformList
    def build(implicit lookup: DefineLookup): Pattern
  }

  case class CheckersPatternValue(
    a: Color, b: Color,
    transform: TransformList) extends PatternValue {
    override def build(implicit lookup: DefineLookup): Pattern =
      CheckersPattern(a, b, transform.build)
  }

  case class StripePatternValue(
    a: Color, b: Color,
    transform: TransformList) extends PatternValue {
    override def build(implicit lookup: DefineLookup): Pattern =
      StripePattern(a, b, transform.build)
  }

  case class GradientPatternValue(
    a: Color, b: Color,
    transform: TransformList) extends PatternValue {
    override def build(implicit lookup: DefineLookup): Pattern =
      GradientPattern(a, b, transform.build)
  }

  // Yaml "- add: type"
  trait AddObject extends YamlItem

  case class CameraObject(
    width: Int,
    height: Int,
    fieldOfView: Double,
    from: Point3D,
    to: Point3D,
    up: Vector3D) extends AddObject {
    def build: Camera = Camera(width, height, fieldOfView, Operations.viewTransform(from, to, up))
  }

  case class LightObject(
    at: Point3D,
    intensity: Color) extends AddObject {
    def build: PointLight = PointLight(at, intensity)
  }

  trait ShapeObject extends AddObject {
    def material: Option[MaterialObject]
    def transform: TransformList

    def buildMaterial(parent: Option[MaterialObject])(implicit lookup: DefineLookup): Material =
      (material orElse parent).fold(Material())(_.build)
    def buildTransform(parent: Option[TransformList])(implicit lookup: DefineLookup): Matrix =
      transform.append(parent).build
    def build(
      parentMaterial: Option[MaterialObject],
      parentTransform: Option[TransformList])(implicit lookup: DefineLookup): Shape
  }
  case class SimpleShapeObject(
    shapeType: String,
    material: Option[MaterialObject],
    transform: TransformList) extends ShapeObject {

    override def build(
      parentMaterial: Option[MaterialObject],
      parentTransform: Option[TransformList])(implicit lookup: DefineLookup): Shape = {
      val transformMatrix = buildTransform(parentTransform)
      val mat = buildMaterial(parentMaterial)
      shapeType match {
        case "plane" => Shapes.plane(transformMatrix, mat)
        case "sphere" => Shapes.sphere(transformMatrix, mat)
        case "cube" => Shapes.cube(transformMatrix, mat)
        case other =>
          throw new RuntimeException(s"Shape type not supported: $other")
      }
    }
  }

  case class ShapesGroup(
    material: Option[MaterialObject],
    transform: TransformList,
    children: Seq[ShapeObject]) extends ShapeObject {
    override def build(
      parentMaterial: Option[MaterialObject],
      parentTransform: Option[TransformList])(implicit lookup: DefineLookup): Shape = {
      val transformMatrix = buildTransform(parentTransform)
      val group = Shapes.group(transformMatrix)
      //TODO: figure out material????
      children.foreach { child =>
        val shape = child.build(parentMaterial, None) //Dont pass in transform (group handles that)
        group.add(shape)
      }
      group
    }
  }

  case class CylindricObject(
    material: Option[MaterialObject],
    transform: TransformList,
    minimum: Double,
    maximum: Double,
    closed: Boolean,
    isCylinder: Boolean) extends ShapeObject {

    def build(
      parentMaterial: Option[MaterialObject],
      parentTransform: Option[TransformList])(implicit lookup: DefineLookup): Shape = {
      val transformMatrix = buildTransform(parentTransform)
      val mat = buildMaterial(parentMaterial)
      if (isCylinder) Shapes.cylinder(transformMatrix, mat, minimum, maximum, closed)
      else Shapes.cone(transformMatrix, mat, minimum, maximum, closed)
    }
  }

  case class ShapeReference(
    reference: String,
    material: Option[MaterialObject],
    transform: TransformList) extends ShapeObject {
    def build(
      parentMaterial: Option[MaterialObject],
      parentTransform: Option[TransformList])(implicit lookup: DefineLookup): Shape = {
      println(s"Resolving $reference")
      lookup.shapes.get(reference) match {
        case Some(shape) =>
          val combined = transform.append(parentTransform)
          shape.value.build(parentMaterial orElse material, Some(combined))
        case None =>
          println(s"reference not found: $reference!")
          throw new RuntimeException(s"shape reference not found: $reference")
      }
    }
  }

  case class ObjFileShape(
    material: Option[MaterialObject],
    transform: TransformList,
    filename: String) extends ShapeObject {
    def build(
      parentMaterial: Option[MaterialObject],
      parentTransform: Option[TransformList])(implicit lookup: DefineLookup): Shape = {

      val transformMatrix = buildTransform(parentTransform)
      val mat = buildMaterial(parentMaterial)
      lookup.loader.loadObject(filename)
          .setMaterial(mat)
          .setTransform(transformMatrix)
    }
  }

  case class SceneAst(items: Seq[YamlItem]) {
    def adds: Seq[AddObject] = items.collect { case x: AddObject => x }
    def shapes: Seq[ShapeObject] = adds.collect { case x: ShapeObject => x }
    def lights: Seq[LightObject] = adds.collect { case x: LightObject => x }
    def cameras: Seq[CameraObject] = adds.collect { case x: CameraObject => x }
    def build(implicit resourceLoader: ResourceLoader): Scene = {
      implicit val lookup: DefineLookup = DefineLookup(items)
      val camera = cameras.headOption.map(_.build).getOrElse(Defaults.camera)
      val Point3DLights = lights.map(_.build)
      val objects = shapes.map(_.build(None, None))
      Scene(World(objects, Point3DLights), camera)
    }
  }

  case class DefineLookup(
    materials: Map[String, DefineMaterial],
    transforms: Map[String, DefineTransform],
    shapes: Map[String, DefineShape],
    loader: ResourceLoader)

  object DefineLookup {
    private def makeLookup[A <: DefineObject](xs: Seq[A]) = xs.map(x => x.key -> x).toMap

    def apply(items: Seq[YamlItem])(implicit loader: ResourceLoader): DefineLookup = {
      val defines: Seq[DefineObject] = items.collect { case x: DefineObject => x }
      val materials = makeLookup(defines.collect { case m: DefineMaterial => m })
      val transforms = makeLookup(defines.collect { case m: DefineTransform => m })
      val shapes = makeLookup(defines.collect { case m: DefineShape => m })
      new DefineLookup(materials, transforms, shapes, loader)
    }

    def empty(implicit loader: ResourceLoader): DefineLookup = DefineLookup(Nil)
  }




}
