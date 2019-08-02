package raytracer.waveform

// Waveform Obj Ast
sealed trait ObjAst
object ObjAst {
  case class Vertex(x: Double, y: Double, z: Double, w: Double) extends ObjAst
  case class VertexNormal(x: Double, y: Double, z: Double) extends ObjAst

  trait Face extends ObjAst
  case class FaceVertex(xs: Seq[Int]) extends Face
  case class FaceVertexNormal(xs: Seq[(Int, Int)]) extends Face
  case class FaceVertexTextureNormal(xs: Seq[(Int, Int, Int)]) extends Face

  case class GroupName(name: String) extends ObjAst
}

