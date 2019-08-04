package raytracer.waveform

trait ObjBuilder {

  def addVector(x: Double, y: Double, z: Double, w: Double): Unit
  def addVertexNormal(x: Double, y: Double, z: Double): Unit
  def textureCoordinates(u: Double, v: Option[Double], w: Option[Double]): Unit
  def addFace(xs: Seq[Int]): Unit
  def addFace2(xs: Seq[(Int, Int)]): Unit
  def addFace3(xs: Seq[(Int, Int, Int)]): Unit
  def addGroup(name: String): Unit
  def addObject(name: String): Unit
  def useMaterial(material: String): Unit
  def useMaterialLib(materialLibFile: String): Unit

}
