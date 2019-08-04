package raytracer

import raytracer.shapes.Shape

trait Pattern {
  type PatternType <: Pattern

  def colorAt(worldPoint: Point3D, obj: Shape): Color
  def colorAt(point: Point3D): Color
  def transform: Matrix
  def setTransform(t: Matrix): PatternType
}
