package raytracer.resource.yaml

import raytracer.Color
import raytracer.math.{Operation, Point3D, Vector3D}

object AST {

  sealed trait YamlValue

  // Transforms
  case class TransformValue(xs: Seq[TransformItem])
  sealed trait TransformItem
  case class TransformReference(key:String) extends TransformItem
  case class TransformOperation(op: Operation) extends TransformItem

  // Patterns
  sealed trait PatternValue
  case class CheckersPatternValue( a: Color, b: Color, transform: TransformValue) extends PatternValue
  case class StripePatternValue(   a: Color, b: Color, transform: TransformValue) extends PatternValue
  case class GradientPatternValue( a: Color, b: Color, transform: TransformValue) extends PatternValue
  case class RingPatternValue(     a: Color, b: Color, transform: TransformValue) extends PatternValue

  // Material
  trait MaterialValue
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
  sealed trait DefineValue extends YamlValue
  case class DefineMaterial(key:String, extend: Option[String], value: MaterialValue) extends DefineValue
  case class DefineTransform(key:String, value: TransformValue) extends DefineValue
  case class DefineShape(key:String, value: ShapeValue) extends DefineValue

  // Adds
  trait AddValue extends YamlValue
  case class AddCamera(
    width: Int, height: Int, fieldOfView: Double,
    from: Point3D, to: Point3D, up: Vector3D) extends AddValue
  case class AddLight(at: Point3D, intensity: Color) extends AddValue

  // Add shapes
  trait AddShape extends AddValue
  case class ShapeReference(
    key: String,
    transform: TransformValue,
    material: Option[MaterialValue]) extends AddShape

  // Shape values
  sealed trait ShapeValue extends AddShape
  case class CylinderValue(
    minimum: Double, maximum: Double, closed: Boolean,
    transform: TransformValue, material: Option[MaterialValue]) extends ShapeValue
  case class ConeValue(
    minimum: Double, maximum: Double, closed: Boolean,
    transform: TransformValue, material: Option[MaterialValue]) extends ShapeValue
  case class SphereValue(transform: TransformValue, material: Option[MaterialValue]) extends ShapeValue
  case class CubeValue(transform: TransformValue, material: Option[MaterialValue]) extends ShapeValue
  case class PlaneValue(transform: TransformValue, material: Option[MaterialValue]) extends ShapeValue
  case class ObjFileValue(filename: String, transform: TransformValue, material: Option[MaterialValue]) extends ShapeValue
  case class GroupValue(children: Seq[AddShape], transform: TransformValue, material: Option[MaterialValue]) extends ShapeValue

}
