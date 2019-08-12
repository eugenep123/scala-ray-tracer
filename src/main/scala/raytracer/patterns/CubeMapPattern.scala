package raytracer.patterns
import raytracer.Color
import raytracer.math.{Matrix, Point3D}
import raytracer.shapes.Cube
import raytracer.shapes.Cube.Face

case class CubeMapPattern(
  left: UVPattern,
  front: UVPattern,
  right: UVPattern,
  back: UVPattern,
  up: UVPattern,
  down: UVPattern,
  transform: Matrix) extends Pattern {

  def patternForFace(face: Face): UVPattern = {
    face match {
      case Face.Left => left
      case Face.Front => front
      case Face.Right => right
      case Face.Back => back
      case Face.Up => up
      case Face.Down => down
    }
  }

  override def colorAt(point: Point3D): Color = {
    val face = Cube.face(point)
    val (u, v) = UVMapping.Cube.map(face, point)
    val pattern = patternForFace(face)
    pattern.colorAt(u, v)
  }

  override type PatternType = CubeMapPattern
  override def setTransform(t: Matrix): CubeMapPattern = this.copy(transform = t)
}

object CubeMapPattern {
  def apply(
    left: UVPattern,
    front: UVPattern,
    right: UVPattern,
    back: UVPattern,
    up: UVPattern,
    down: UVPattern,
    transform: Matrix = Matrix.identity): CubeMapPattern =
    new CubeMapPattern(left, front, right, back, up, down, transform)
}
