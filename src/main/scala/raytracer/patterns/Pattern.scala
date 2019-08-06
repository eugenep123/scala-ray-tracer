package raytracer.patterns

import raytracer.Color
import raytracer.math.{Matrix, Point3D}
import raytracer.shapes.Shape

abstract class Pattern {

  final def colorAt(worldPoint: Point3D, obj: Shape): Color = {
    val objectPoint = obj.transform.inverse * worldPoint
    val patternPoint = transform.inverse * objectPoint
    colorAt(patternPoint)
  }

  def colorAt(point: Point3D): Color
  def transform: Matrix

  type PatternType <: Pattern
  def setTransform(t: Matrix): PatternType
}