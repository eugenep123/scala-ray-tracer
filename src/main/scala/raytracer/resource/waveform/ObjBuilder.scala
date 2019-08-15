package raytracer.resource.waveform

import performance.obj.{Face, ObjResult, VertexIndex}
import raytracer.math.{Point3D, Radians, Vector3D}

class ObjBuilder extends ObjectHandler[ObjResult] {
  type UVPair = (Double, Double)
  private var materials = Vector.empty[String]
  private var groups = Vector("default-group")
  private var objects = Vector.empty[String]
  private var vertices = Vector.empty[Point3D]
  private var normals = Vector.empty[Vector3D]
  private var textureCoords = Vector.empty[UVPair]
  private var faces = Vector.empty[Face]

  private def groupId: Int = groups.size - 1
  private def materialId: Int = materials.size - 1

  override def onGroup(name: String): Unit = {
    if (faces.isEmpty && groups.size == 1) {
      groups = Vector.empty
    }
    groups :+= name
  }

  override def onObject(name: String): Unit = {
    objects :+= name
  }

  override def onVertex(x: Double, y: Double, z: Double, w: Option[Double]): Unit = {
    vertices :+= Point3D(x, y, z)
  }

  override def onNormal(x: Double, y: Double, z: Double): Unit = {
    normals :+= Vector3D(x, y, z)
  }

  override def onTextureCoords(u: Radians, v: Radians, w: Option[Radians]): Unit = {
    textureCoords :+= (u, v)
  }
  override def onFace(xs: Seq[VertexIndex]): Unit = {
    faces :+= Face(materialId, groupId, xs)
  }

  override def onUseMaterial(name: String): Unit = {
    materials :+= name
  }

  override def onSetSmoothShading(on: Boolean): Unit = {
    println(s"Set Smooth shading : $on")
  }

  override def onMaterialLib(filename: String): Unit = {
    println(s"Material lib: $filename")
  }


  override def result(): ObjResult = {
    ObjResult(materials, groups, vertices, normals, textureCoords, faces)
  }
}
