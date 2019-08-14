package raytracer.resource.waveform

import raytracer.Color


// http://paulbourke.net/dataformats/mtl/
class MaterialLibParser[A](handler: MaterialHandler[A]) {
  import fastparse._
  import NoWhitespace._
  import SharedParsers._
  import handler._

  private def color[_:P]: P[Color] = triple(decimal, " ").map((Color.apply _).tupled)
  private def newMaterial[_: P]: P[Unit] = P("newmtl " ~/  name).map(onNewMaterial)
  private def specularExponent[_: P]: P[Unit] = P("Ns " ~/  decimal).map(onSpecularExponent)
  private def refractionIndex[_: P]: P[Unit] = P("Ni " ~/  decimal).map(onRefractionIndex)
  private def dissolve[_: P]: P[Unit] = P("d " ~/  decimal).map(onTransparent)
  private def dissolveInv[_: P]: P[Unit] = P("Tr " ~/ decimal ~ "  0".?).map(d => onTransparent(1.0 - d))
  private def illuminationModel[_: P]: P[Unit] = P("illum " ~/  index).map(onIlluminationModel)
  private def ambientColor[_: P]: P[Unit] = P("Ka " ~/  color).map(onAmbientColor)
  private def diffuseColor[_: P]: P[Unit] = P("Kd " ~/  color).map(onDiffuseColor)
  private def specularColor[_: P]: P[Unit] = P("Ks " ~/  color).map(onSpecularColor)
  private def emissiveColor[_: P]: P[Unit] = P("Ke " ~/  color).map(onEmissiveColor)
  private def diffuseMap[_: P]: P[Unit] = P("map_Kd " ~/  name).map(onDiffuseMap)

  private def line[_:P]: P[Unit] = P(
    newMaterial | specularExponent | refractionIndex | dissolve | dissolveInv |
      illuminationModel | ambientColor | diffuseColor | specularColor | emissiveColor |
      diffuseMap | comment | ""
  )
  private def lines[_:P]: P[Unit] = P( spaceOrNewlines ~/ ("  ".? ~ line).rep(1, newLine) ~ spaceOrNewlines.? ~ End).log

  def parse(input: String): A = {
    val parsed = fastparse.parse(input, lines(_), startIndex = 0)
    val Parsed.Success(value, successIndex) = parsed
    handler.result()
  }
}
