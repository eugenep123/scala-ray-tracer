package raytracer.patterns
import raytracer.Color
import raytracer.math.{Matrix, Point3D}

case class TextureMapPattern(
  uv: UVPattern,
  mapping: UVMapping,
  transform: Matrix) extends Pattern {
  override type PatternType = TextureMapPattern

  override def patternAt(point: Point3D): Color = {
    val (u, v) = mapping.map(point)
    uv.colorAt(u, v)
  }

  override def setTransform(t: Matrix): TextureMapPattern = this.copy(transform = t)
}

object TextureMapPattern {
  def apply(
             uv: UVPattern,
             mapping: UVMapping,
             transform: Matrix = Matrix.identity): TextureMapPattern =
    new TextureMapPattern(uv, mapping, transform)
}
