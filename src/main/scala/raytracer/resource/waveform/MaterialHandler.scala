package raytracer.resource.waveform

import raytracer.Color

trait MaterialHandler[A] {
  def onNewMaterial(name: String): Unit
  def onSpecularExponent(d: Double): Unit
  def onRefractionIndex(d: Double): Unit
  def onTransparent(d: Double): Unit
  def onIlluminationModel(n: Int): Unit
  def onAmbientColor(c: Color): Unit
  def onDiffuseColor(c: Color): Unit
  def onSpecularColor(c: Color): Unit
  def onEmissiveColor(c: Color): Unit
  def onDiffuseMap(filename: String): Unit
  def result(): A
}
object MaterialHandler {

  val print = new MaterialHandler[Unit] {

    override def onNewMaterial(name: String): Unit = {
      println(s"New material: $name")
    }

    override def onSpecularExponent(d: Double): Unit = {
      println(s"Specular exponent: $d")
    }

    override def onRefractionIndex(d: Double): Unit = {
      println(s"Refraction index: $d")
    }

    override def onTransparent(d: Double): Unit = {
      println(s"Transparent: $d")
    }

    override def onIlluminationModel(n: Int): Unit = {
      println(s"Illumination model: $n")
    }

    override def onAmbientColor(c: Color): Unit = {
      println(s"ambient color: $c")
    }

    override def onDiffuseColor(c: Color): Unit = {
      println(s"diffuse color: $c")
    }

    override def onSpecularColor(c: Color): Unit = {
      println(s"specular color: $c")
    }

    override def onEmissiveColor(c: Color): Unit = {
      println(s"Emissive color: $c")
    }


    override def onDiffuseMap(filename: String): Unit = {
      println(s"Diffuse map: $filename")
    }

    override def result(): Unit = {}
  }
}
