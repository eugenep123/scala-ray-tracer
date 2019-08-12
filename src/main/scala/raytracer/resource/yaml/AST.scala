package raytracer.resource.yaml

import raytracer.Color
import raytracer.math.{Point3D, Transform, Vector3D}
import raytracer.patterns.UVMapping

object AST {

  sealed trait YamlValue

  // Transforms
  type TransformOption = Option[TransformList]
  case class TransformList(xs: Seq[TransformItem])
  sealed trait TransformItem
  case class TransformReference(key:String) extends TransformItem
  case class TransformOperation(op: Transform) extends TransformItem

  // Patterns
  sealed trait PatternValue {
    def transform: TransformOption
  }
  case class CheckersPatternValue( a: Color, b: Color, transform: TransformOption) extends PatternValue
  case class StripePatternValue(   a: Color, b: Color, transform: TransformOption) extends PatternValue
  case class GradientPatternValue( a: Color, b: Color, transform: TransformOption) extends PatternValue
  case class RingPatternValue(     a: Color, b: Color, transform: TransformOption) extends PatternValue
  case class MapPatternValue(mapping: UVMapping, uv: UVPatternValue, transform: TransformOption) extends PatternValue
  case class CubeMapPatternValue(
    left: UVPatternValue,
    front: UVPatternValue,
    right: UVPatternValue,
    back: UVPatternValue,
    up: UVPatternValue,
    down: UVPatternValue,
    transform: TransformOption) extends PatternValue

  sealed trait UVPatternValue
  case class AlightCheckUVPatternValue(main: Color, ul: Color, ur: Color, bl: Color, br: Color) extends UVPatternValue
  case class CheckersUVPatternValue(width: Int, height: Int, a: Color, b: Color) extends UVPatternValue
  case class ImageUVPatternValue(file: String) extends UVPatternValue

  // Material
  type MaterialOption = Option[MaterialValue]
  type MaterialList = Seq[MaterialValue]
  sealed trait MaterialValue
  case class MaterialReference(key: String) extends MaterialValue
  case class MaterialObject(
    color: Option[Color] = None,
    diffuse: Option[Double] = None,
    ambient: Option[Double] = None,
    specular: Option[Double] = None,
    shininess: Option[Double] = None,
    reflective: Option[Double] = None,
    transparency: Option[Double] = None,
    refractiveIndex: Option[Double] = None,
    pattern: Option[PatternValue] = None) extends MaterialValue

  // Defines
  sealed trait DefineValue extends YamlValue {
    def key: String
  }
  case class DefineMaterial(key: String, extend: Option[String], value: MaterialObject) extends DefineValue
  case class DefineTransform(key: String, value: TransformList) extends DefineValue
  case class DefineShape(key: String, value: AddShape) extends DefineValue

  // Adds
  sealed trait AddValue extends YamlValue
  case class AddCamera(
    width: Int, height: Int, fieldOfView: Double,
    from: Point3D, to: Point3D, up: Vector3D) extends AddValue
  case class AddLight(at: Point3D, intensity: Color) extends AddValue

  // Add shapes (all shapes have transform and material)
  sealed trait AddShape extends AddValue {
    def transform: TransformOption
    def material: MaterialOption
  }
  case class ShapeReference(
    key: String,
    transform: TransformOption,
    material: MaterialOption) extends AddShape
  case class AddCylinder(
    minimum: Double, maximum: Double, closed: Boolean,
    transform: TransformOption, material: MaterialOption) extends AddShape
  case class AddCone(
    minimum: Double, maximum: Double, closed: Boolean,
    transform: TransformOption, material: MaterialOption) extends AddShape
  case class AddSphere(transform: TransformOption, material: MaterialOption) extends AddShape
  case class AddCube(
    transform: TransformOption,
    material: MaterialOption,
    shadow: Boolean) extends AddShape
  case class AddPlane(transform: TransformOption, material: MaterialOption) extends AddShape
  case class AddObjFile(file: String, transform: TransformOption, material: MaterialOption) extends AddShape
  case class AddGroup(
    children: Seq[AddShape],
    transform: TransformOption,
    material: MaterialOption) extends AddShape


  // AST helpers
  case class DefineLookup (
    materials: Map[String, DefineMaterial],
    transforms: Map[String, DefineTransform],
    shapes: Map[String, DefineShape])

  case class MaterialObjectBuilder(obj: MaterialObject) {
    protected def update(f: MaterialObject => MaterialObject) =
      MaterialObjectBuilder(f(obj))
    def color(c: Color): MaterialObjectBuilder = update(_.copy(color = Some(c)))
    def color(r: Double, g: Double, b: Double): MaterialObjectBuilder =
      color(Color(r, g, b))
    def diffuse(x: Double): MaterialObjectBuilder = update(_.copy(diffuse = Some(x)))
    def ambient(x: Double): MaterialObjectBuilder = update(_.copy(ambient = Some(x)))
    def specular(x: Double): MaterialObjectBuilder = update(_.copy(specular = Some(x)))
    def shininess(x: Double): MaterialObjectBuilder = update(_.copy(shininess = Some(x)))
    def reflective(x: Double): MaterialObjectBuilder = update(_.copy(reflective = Some(x)))
    def transparency(x: Double): MaterialObjectBuilder = update(_.copy(transparency = Some(x)))
    def refractiveIndex(x: Double): MaterialObjectBuilder = update(_.copy(refractiveIndex = Some(x)))
    def pattern(p: PatternValue): MaterialObjectBuilder = update(_.copy(pattern = Some(p)))


    protected def set[A](opt: Option[A])(withF: MaterialObjectBuilder => A => MaterialObjectBuilder): MaterialObjectBuilder = {
      opt.fold(this)(a => withF(this)(a))
    }
    def set(m: MaterialObject): MaterialObjectBuilder = {
      set(m.color)(_.color)
        .set(m.diffuse)(_.diffuse)
        .set(m.ambient)(_.ambient)
        .set(m.specular)(_.specular)
        .set(m.shininess)(_.shininess)
        .set(m.reflective)(_.reflective)
        .set(m.transparency)(_.transparency)
        .set(m.refractiveIndex)(_.refractiveIndex)
        .set(m.pattern)(_.pattern)
    }
  }

  object MaterialObjectBuilder {
    def apply(): MaterialObjectBuilder = new MaterialObjectBuilder(MaterialObject())
    implicit def builderToObj(b: MaterialObjectBuilder): MaterialObject = b.obj
  }
}
