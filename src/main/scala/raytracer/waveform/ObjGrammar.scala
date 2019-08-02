package raytracer.waveform

import fastparse._
import NoWhitespace._
import ObjAst._

// https://en.wikipedia.org/wiki/Wavefront_.obj_file
object ObjGrammar {

  type Double3 = (Double, Double, Double)
  type Double4 = (Double, Double, Double, Double)

  def spaces[_: P] = P(CharsWhileIn(" \t", 0))
  def newLine[_: P] = P(spaces ~ CharsWhileIn("\n\r", 1) ~ spaces)
  def spaceOrNewlines[_: P] = P(CharsWhileIn(" \r\n", 0))
  def digits[_: P] = P(CharsWhileIn("0-9"))
  def exponent[_: P] = P(CharIn("eE") ~ CharIn("+\\-").? ~ digits)
  def fractional[_: P] = P("." ~ digits)
  def integral[_: P] = P("0" | CharIn("1-9") ~ digits.?)

  def decimal[_: P] =
    P(CharIn("+\\-").? ~ integral ~ fractional.? ~ exponent.?).!.map(
      x => x.toDouble
    ).log

  def index[_: P] =
    integral.!.map(_.toInt) // P( CharIn("1-9") ~ digits.? ).!.map(_.toInt)
  def name[_: P]: P[String] = P(CharsWhileIn("_()0-9a-zA-Z\\-", 1).!)

  def doubleValue[_: P] = (spaces ~ decimal ~ &(spaceOrNewlines))

  // Vector: (x, y, z [,w]) coordinates, w is optional and defaults to 1.0.
  // v 0.123 0.234 0.345 1.0
  def vector[_: P]: P[Double4] = P(
    "v " ~/ (spaces ~ decimal) ~ (spaces ~ decimal) ~ (spaces ~ decimal) ~ (spaces ~ decimal).?.map(
      _.getOrElse(1.0))
  )

  //  # List of vertex normals in (x,y,z) form; normals might not be unit vectors.
  //  vn 0.707 0.000 0.707
  def vertexNormal[_: P]: P[Double3] =
    P("vn " ~/ (spaces ~ decimal) ~ (spaces ~ decimal) ~ (spaces ~ decimal))

  // # Polygonal face element (see below)
  //f 1 2 3
  def faceVertex[_: P]: P[Seq[Int]] = P("f " ~ (spaces ~ index).rep(1))

  // #Face with vertex normals: f v1/vt1 v2/vt2 v3/vt3 ...
  //  1//3 2//1 3//2
  def faceVertexNormal[_: P]: P[Seq[(Int, Int)]] =
    P("f " ~ (spaces ~ index ~ "//" ~ index).rep(1))

  // f v1/vt1/vn1 v2/vt2/vn2 v3/vt3/vn3 ...
  def faceVertexTextureNormal[_: P]: P[Seq[(Int, Int, Int)]] =
    P("f " ~ spaces ~ (index ~ "/" ~ index ~ "/" ~ index).rep(1, spaces)).log

  // Group: g {name}
  // g FirstGroup
  def group[_: P]: P[String] = P("g " ~/ spaces ~ name ~ &(spaceOrNewlines)).log

  def comment[_: P]: P[Unit] = P(spaces.? ~ "#" ~/ CharsWhile(_ != '\n'))

  // Model mapping
  def face[_: P]: P[Face] = P(
    faceVertex.map(FaceVertex.apply) |
      faceVertexNormal.map(FaceVertexNormal.apply) |
      faceVertexTextureNormal.map(FaceVertexTextureNormal.apply)
  )

  def model[_: P]: P[ObjAst] =
    P(
      vector.map((Vertex.apply _).tupled) |
        vertexNormal.map((VertexNormal.apply _).tupled) |
        face |
        group.map(GroupName.apply)
    ).log

  def line[_: P]: P[Option[ObjAst]] = P(
    spaces ~ (model.map(Option.apply) | comment.map(_ => Option.empty[ObjAst]))
  )

  def lines[_: P] =
    P(
      spaceOrNewlines ~ line.rep(1, newLine) ~ spaceOrNewlines ~ End
    ).map(xs => xs.flatten)

}
