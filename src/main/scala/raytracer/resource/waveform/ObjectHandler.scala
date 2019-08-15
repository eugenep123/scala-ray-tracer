package raytracer.resource.waveform

import performance.obj.VertexIndex
import raytracer.math.{Point3D, Vector3D}

trait ObjectHandler[A] {
  def onGroup(name: String): Unit
  def onObject(name: String): Unit
  def onVertex(x: Double, y: Double, z: Double, w: Option[Double]): Unit
  def onTextureCoords(u: Double, v: Double, w: Option[Double]): Unit
  def onNormal(x: Double, y: Double, z: Double): Unit
  def onFace(xs: Seq[VertexIndex]): Unit
  def onSetSmoothShading(on: Boolean): Unit
  def onMaterialLib(filename: String): Unit
  def onUseMaterial(name: String): Unit
  def result(): A
}

object ObjectHandler {
  def printHandler: ObjectHandler[Unit]  = new ObjectHandler[Unit] {
    override def onVertex(x: Double, y: Double, z: Double, w: Option[Double]): Unit = {
      val p = Point3D(x, z, y)
      println(s"vertex: $p")
    }

    override def onGroup(name: String): Unit = {
      println(s"Group: $name")
    }

    override def onNormal(x: Double, y: Double, z: Double): Unit = {
      val v = Vector3D(x, z, y)
      println(s"normal: $v")
    }

    override def onObject(name: String): Unit = {
      println(s"Object: $name")
    }

    override def onFace(xs: Seq[VertexIndex]): Unit = {
      println(s"Face: ${xs.mkString(", ")}")
    }

    override def onSetSmoothShading(on: Boolean): Unit = {
      println(s"Smooth shading: $on")
    }

    override def onMaterialLib(filename: String): Unit = {
      println(s"Material lib: $filename")
    }

    override def onUseMaterial(name: String): Unit = {
      println(s"Use material: $name")
    }

    override def onTextureCoords(u: Double, v: Double, w: Option[Double]): Unit = {
      println(s"texture coords: $u, $v")
    }

    override def result(): Unit = {}
  }
}
