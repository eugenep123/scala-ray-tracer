package raytracer.waveform

import fastparse._
import NoWhitespace._

// https://en.wikipedia.org/wiki/Wavefront_.obj_file
object ObjGrammar {

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
    )

  def index[_: P] =
    integral.!.map(_.toInt) // P( CharIn("1-9") ~ digits.? ).!.map(_.toInt)
  def name[_: P]: P[String] = P(CharsWhileIn("_()0-9a-zA-Z\\-", 1).!)
  def spaceDouble[_:P] = P((spaces ~ decimal))

  // Group: g {name}
  // g FirstGroup
  def group[_: P]: P[String] = P("g " ~/ spaces ~ name ~ &(spaceOrNewlines))

  def useMaterial[_:P] = P("usemtl " ~/ spaces ~ name ~ &(spaceOrNewlines))
  // # until \n
  def comment[_: P]: P[Unit] = P(spaces.? ~ "#" ~/ CharsWhile(_ != '\n').?)


  def setSmoothShading[_:P] = P("s " ~/ spaces ~ ("1" | "off"))

  // Vector: (x, y, z [,w]) coordinates, w is optional and defaults to 1.0.
  // v 0.123 0.234 0.345 1.0
  def vector[_: P]: P[(Double, Double, Double, Double)] =
    P("v " ~/ spaceDouble ~ spaceDouble ~ spaceDouble ~ spaceDouble.?.map(_.getOrElse(1.0)))

  //  # List of vertex normals in (x,y,z) form; normals might not be unit vectors.
  //  vn 0.707 0.000 0.707
  def vertexNormal[_: P]: P[(Double, Double, Double)] =
    P("vn " ~/ spaceDouble ~ spaceDouble ~ spaceDouble)

  //  # List of texture coordinates, in (u, [v ,w]) coordinates, these will vary between 0 and 1, v and w are optional and default to 0.
  //  vt 0.500 1 [0]
  def textureCoordinates[_:P]: P[(Double, Option[Double], Option[Double])] =
    P("vt " ~/ spaceDouble ~ spaceDouble.? ~ spaceDouble.?)

  // # Polygonal face element (see below)
  def vertexIndices[_:P]: P[Seq[Int]] = (spaces ~ index).rep(3)
  //  1//3 2//1 3//2
  def vertexNormalIndices[_: P]: P[Seq[(Int, Int)]] = (spaces ~ index ~ "//" ~ index).rep(3)
  //v1/vt1/vn1 v2/vt2/vn2 v3/vt3/vn3 ...
  def vertexTextureNormalIndices[_:P]: P[Seq[(Int, Int, Int)]] = (index ~ "/" ~ index ~ "/" ~ index).rep(3, spaces)

  def face[_:P] = P("f " ~/ (
    vertexIndices |
    vertexNormalIndices |
    vertexTextureNormalIndices
  ))

  def faceBuilder[_:P](builder: ObjBuilder) = P("f " ~/ (
    vertexIndices.map(builder.addFace(_)) |
      vertexNormalIndices.map(builder.addFace2(_)) |
      vertexTextureNormalIndices.map(builder.addFace3(_))
    ))

  def itemBuilder[_:P](builder: ObjBuilder) = P(
    vector.map((builder.addVector _).tupled) |
    vertexNormal.map((builder.addVertexNormal _).tupled) |
    faceBuilder(builder) |
    group.map(builder.addGroup) |
    useMaterial.map(builder.useMaterial) |
    textureCoordinates.map((builder.textureCoordinates _).tupled) |
    setSmoothShading //ignore
  )

  def builderParser[_:P](builder: ObjBuilder): P[Unit] = P(
    spaceOrNewlines ~ (comment | itemBuilder(builder)).rep(1, newLine) ~ spaceOrNewlines ~ End
  )


}
