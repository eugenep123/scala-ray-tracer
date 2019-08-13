package raytracer.patterns

import raytracer.Color
import raytracer.math.{Matrix, Point3D}
import raytracer.shapes.Shape

abstract class Pattern {

  final def patternAtShape(worldPoint: Point3D, obj: Shape): Color = {
    val objectPoint = obj.transform.inverse * worldPoint
    val patternPoint = transform.inverse * objectPoint
    patternAt(patternPoint)
  }

  def patternAt(point: Point3D): Color
  def transform: Matrix

  type PatternType <: Pattern
  def setTransform(t: Matrix): PatternType
}

object Pattern {

  def stripe(a: Color, b: Color): StripePattern = StripePattern(a, b)
  def gradient(a: Color, b: Color): GradientPattern = GradientPattern(a, b)
  def checkers(a: Color, b: Color): CheckersPattern = CheckersPattern(a, b)
  def ring(a: Color, b: Color): RingPattern = RingPattern(a, b)
  def textureMap(uv: UVPattern, mapping: UVMapping): TextureMapPattern = TextureMapPattern(uv, mapping)
  def cubeMap(
    left: UVPattern,
    front: UVPattern,
    right: UVPattern,
    back: UVPattern,
    up: UVPattern,
    down: UVPattern): CubeMapPattern = CubeMapPattern(left, front, right, back, up, down)
}