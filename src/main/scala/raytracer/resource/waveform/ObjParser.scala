package raytracer.resource.waveform

import performance.obj.VertexIndex

class ObjParser[A](handler: ObjectHandler[A]) {
  import fastparse._
  import NoWhitespace._
  import SharedParsers._
  import handler._

    // Handlers
  private def group[_: P]: P[Unit] = P("g " ~/  name).map(onGroup)
  private def namedObject[_: P]: P[Unit] = P("o " ~/  name).map(onObject)
  private def textureCoords[_: P]: P[Unit] = P("vt " ~/ decimal ~ " " ~ decimal ~ (" " ~ decimal).?).map((onTextureCoords _).tupled)
  private def vertex[_: P]: P[Unit] = P( ("v " ~ spaces.?) ~/ triple(decimal) ~ (" " ~ decimal).?).map((onVertex _).tupled)
  private def normal[_: P]: P[Unit] = P("vn " ~/ triple(decimal)).map((onNormal _).tupled)
  private def materialLib[_: P]: P[Unit] = P("mtllib " ~/  name).map(onMaterialLib)
  private def useMaterial[_: P]: P[Unit] = P("usemtl " ~/  name).map(onUseMaterial)

  private def indexOnly[_: P]: P[VertexIndex] = P(index).map(i => VertexIndex(i))
  private def indexNormal[_: P]: P[VertexIndex] = P(two(index, "//")).map(pair => VertexIndex(pair._1, -1, pair._2))
  private def indexTexture[_:P]: P[VertexIndex] = P(two(index, "/")).map(pair => VertexIndex(pair._1, pair._2, -1))
  //f v1/vt1/vn1 v2/vt2/vn2 v3/vt3/vn3 ...
  private def indexTextureNormal[_:P]: P[VertexIndex] = P(triple(index, "/")).map((VertexIndex.apply _).tupled)

  private def face[_: P]: P[Unit] = P("f " ~/
    (indexTextureNormal.rep(3, " ") |
    indexNormal.rep(3, " ") |
    indexTexture.rep(3, " ") |
    indexOnly.rep(3, " "))
  ).map(onFace)
  private def smoothShading[_:P]: P[Unit] = P("s " ~ onOrOff).map(onSetSmoothShading)
  private def line[_:P]: P[Unit] = P(
    group | namedObject | vertex | normal | textureCoords | face |
      comment | smoothShading | materialLib | useMaterial |""
  )
  private def lines[_:P]: P[Unit] = P( spaceOrNewlines ~/ line.rep(1, newLine) ~ spaceOrNewlines.? ~ End)

  def parse(input: String): A = {
    val parsed = fastparse.parse(input, lines(_), startIndex = 0)
    val Parsed.Success(value, successIndex) = parsed
    handler.result()
  }
}
